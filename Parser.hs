{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser
    ( -- * Exports
      parseProgram
    , parseExpression
    , parseExpressions
    , parseModule
    , parseInnflutt
    , parseStaðvær
      -- * Re-exports
    , module AST
    )
    where

-- The "<|>" from Applicative is preferential because it has a better precedence
-- level (3) and can thus be mixed with <$>, <*>, <$ and <* with fewer parentheses.
-- TODO: does Parsec 3 define these better?
import Text.Parsec hiding ((<|>), (<?>), many, optional)
import Text.Parsec.String (Parser)
import qualified Text.Parsec as T
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as T

import Control.Applicative (Applicative(..), Alternative(..), (<$>), (<$), (<*), (*>), many, liftA2)
import Control.Monad (replicateM)

import Numeric (readHex)
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, isInfixOf)

import Prelude hiding (init)

import AST
import Language
import Located

-- Redefine <?> with a higher precedence level so that it plays nicely with $.
-- It has a higher precedence level than >> too, which is also useful.
infix 2 <?>
x <?> y = x T.<?> y

-- Eventually this may need a StateT transformer.
type P a = Parser a
          
-- I chose to use the icelandic grammar names contained in the Forritunarmálið
-- Fjölnir Notendahandbók, as a way of making it easier to cross-reference them
-- with the manual.

loc :: P a -> P (Located a)
loc x = do
    pos <- getPosition
    r <- x
    pos' <- getPosition
    return $ L (mkLoc pos `srcSpan` mkLoc pos') r
    where
        mkLoc pos = srcLoc (sourceName pos) (sourceLine pos) (sourceColumn pos)

-- | A character literal, parsing hexadecimal character literals and escaped characters.
staffasti :: P Char
staffasti = lexeme $ between (char '\'') (char '\'') (stafurInner "\\")

-- | A string literal, parsing hexadecimal character literals and escaped characters.
strengur :: P String
strengur = lexeme $ between (char '"') (char '"') (many (stafurInner "\\\""))

-- | Reads a character that can be either in a string or character literal, performing escaping
-- | and hexadecimal literal ('\$41') translation.
stafurInner :: [Char] -> P Char
stafurInner notAllowed = noneOf notAllowed <|> (char '\\' *> (charAsHex <|> escapeChar)) where
    charAsHex = char '$' *> (chr . readHex' <$> replicateM 2 hexDigit)
    escapeChar = choice (zipWith decode "cbefnt0123456789" "\r\a\ESC\f\n\t\0\1\2\3\4\5\6\7\8\9") <|> noneOf "$"
    decode c r = r <$ char c

-- | A version of readHex that assumes the parse cannot fail. If it does fail, an exception is
-- | thrown.
readHex' :: Integral a => String -> a
readHex' = fst . head . readHex

-- TODO: Fail on numbers which can't fit in 16 bits
-- | A positive integral number, i.e. a natural number. This is required at some points in the
-- | grammar, such as in "stef" expressions.
fjöldatala :: P Integer
fjöldatala = lexeme $ readHex' <$> (char '$' *> many1 hexDigit)
                   <|> read <$> many1 digit
                   <?> "natural number"

-- | An integer. The actual type this represents is limited to the range [-32768,32767], but this
-- | parser does not yet check this.
heiltala :: P Integer
heiltala = lexeme (char '-') *> (negate <$> heiltala) <|> fjöldatala <?> "integer"

-- | A floating point number.
-- TODO: use a strictness combinator to make sure read can't possibly blow up
--       at a later point in time due to laziness.
fleytitala :: (RealFrac f, Read f) => P f
fleytitala = (symbol "-" *> fmap negate fleytitala) <|> lexeme core <?> "real number" where
    core = lexeme $ read . concat <$> sequence
        [ many1 digit
        , string "."
        , many digit
        , return "0" -- adding an extra nought satisfies read's parser, since "2.e15" seems to be valid.
        , option "" $ do
            e <- oneOf "eE"
            sign <- fromMaybe '+' <$> optionMaybe (oneOf "+-")
            digits <- many1 digit
            return $ e : sign : digits
        ]

-- Definitions of letters, digits, and operator characters.
--
-- The Notendahandbók doesn't seem to actually define these, but I haven't
-- translated the whole thing, so it may yet turn up. I used the Icelandic
-- alphabet, but it probably wouldn't hurt to allow any character for which
-- isAlpha holds. I also added '_', which the official compiler seems to accept.
nafnstafur, tölustafur, aðgerðarstafur :: P Char
nafnstafur = oneOf alphabet <?> "letter"
tölustafur = digit
aðgerðarstafur = oneOf operatorAlphabet

-- | A variable or subroutine name is a letter followed by a string of letters
-- | and digits.
nafn :: P Name
nafn = T.identifier lexer <?> "name"

-- | An operator name can be either a string of symbols (e.g. "<=") or a
-- | backslash followed by a string of symbols and alphanumeric characters.
-- TODO: Not sure if "try" is necessary. If we find an operator character or
-- a backslash, it pretty much has to be an operator.
aðgerð :: P Name
aðgerð = lexeme $ try $ do
    name <- p1 <|> p2 <?> "operator"
    if name `elem` T.reservedOpNames fjölnirDef
        then unexpected ("reserved operator " ++ showIdent name)
        else return name
    where
        p1 = many1 aðgerðarstafur
        p2 = char '\\' *> (many1 (nafnstafur <|> aðgerðarstafur <|> tölustafur) <?> "rest of named operator")

-- TODO: Figure out exactly what the different module declarations mean, to give
--       the parsers proper labels.
-- | A program is a set of module declarations.
forrit :: P Program
forrit = loc (varEquation <|> stmt) `endBy` semi where
    -- TODO: Figure out what the semantics of these actually are, and if they need different
    -- source tree representations (nafn/strengur).
    -- XXX Yes, they do.
    stmt = do
        name <- loc strengur
        globalEquation name <|> entryPoint name            

    varEquation = DefineModuleVariable 
        <$> loc nafn 
        <*> (reserved "=" *> loc eining)
        <?> "module variable declaration"
        
    globalEquation name = DefineGlobalModule 
        <$> pure name 
        <*> (reserved "=" *> loc eining) 
        <?> "global module declaration"
    
    entryPoint name = DefineEntryPoint 
        <$> pure name 
        <*> (symbol "<" *> loc nafn) 
        <*> loc eining 
        <?> "entry point declaration"

-- | A module
eining :: P ModuleDecl
eining = do
    h <- module'
    t <- many $ (,) <$> modOp <*> module'
    return $ unLoc $ fold h t

    where
        modOp = lexeme $ loc $ (:[]) <$> oneOf "*:+&"

        fold m [] = m
        fold m ((op, m'):rest) =
            let left = combineLoc m m' $ CombinedModule m op m'
            in fold left rest
        
        -- XXX Default compiler does not accept multiple consecutive module operators
        -- without spaces.
        module' =
            loc (symbol "!" *> fmap RecursiveModule module')
            <|> modPrim

        modPrim =  loc (GlobalModule <$> loc strengur)
               <|> loc (ModuleVariable <$> loc nafn)
               <|> loc (brackets eining)
               <|> loc (ModuleDecl <$> loc (braces (many vörpun)))

-- | An entity within a module map.
vörpun :: P (Located Name, ExportDecl)
vörpun = do
    name <- loc nafn <|> loc aðgerð
    symbol "->"
    value <- val name <?> "value of " ++ showIdent (unLoc name)
    return (name, value)
    where val name = nativeImport
                  <|> ExportFunDecl <$> loc (stefskilgreining (unLoc name))
                  <|> ExportAgainDecl <$> loc (nafn <|> aðgerð)
                  <|> ExportVarDecl <$ reserved "breyta"

-- | A native import in a module
-- | Example: f -> cstef openTCPSocket sockets (0;2) ;; addr, port
nativeImport :: P ExportDecl
nativeImport = do
    reserved "cstef"
    cName <- loc nafn
    libName <- loc nafn
    arity <- loc . brackets $ do
        io <- fjöldatala <?> "inout arity of " ++ showIdent (unLoc cName)
        semi <?> "\";\" between number of inout and in parameters for" ++ showIdent (unLoc cName)
        i <- fjöldatala <?> "inward arity of " ++ showIdent (unLoc cName)
        return (io, i)    
    return $ ExportNative cName libName arity

-- | A subroutine entry in a module.
stefskilgreining :: Name -> P FunctionDecl
stefskilgreining name = do
    reserved "stef"
    (inout, in') <- brackets formals <?> "\"(\" after \"stef\""
    vars <- many var
    body <- stofns segðaruna
    return $ FunctionDecl inout in' vars body
    <?> "function declaration"
    where
        var =   ImportVarDecl <$> innflutt
            <|> LocalVarDecl <$> staðvær
        formals = do
            inout <- loc nafn `sepBy` comma <?> "inout parameter declarations for " ++ showIdent name
            semi <?> "\";\" between inout and in parameter declarations for " ++ showIdent name
            in' <- loc nafn `sepBy` comma <?> "inward parameters declarations for " ++ showIdent name
            return (inout, in')

innflutt :: P [Located Name]
innflutt = do
    reserved "innflutt"
    loc nafn `sepBy` comma <?> "comma-separated list of imported names"

staðvær :: P [(Located Name, Maybe (Located Syntax))]
staðvær = try $ reserved "staðvær" *> (sepBy local comma <?> "comma-separated list of local variables")
    where
        local = do
            name <- loc nafn <?> "variable name"
            -- We use reservedOp something like "staðvær x:=+(;1,2)" should be invalid.
            -- "staðvær x:= +(;1,2)", is, however, valid. Similar reasons exist for
            -- all the other instances of ":=" in the parser.
            init <- optionMaybe $ reservedOp ":=" *> (loc segð <?> "initializer expression for " ++ showIdent (unLoc name))
            return (name, init)

-- | An expression.
-- This handles logical operators (ekki, og, eða) and all other operators,
-- along with precedence and associativity rules.
segð :: P Syntax
segð = boolOps andExpr (reserved "eða") OrS <?> "expression" where
    andExpr = boolOps notExpr (reserved "og") AndS
    notExpr = (NotS <$ reserved "ekki" <*> loc notExpr) <|> opExpr precedences

    -- TODO: foldr1? foldl1 seems best, though. We want to evaluate from left to right.
    -- Parse a list of subs separated by delim, folding them together with cons
    boolOps sub delim cons = unLoc . foldl1 f <$> loc sub `sepBy1` delim
        where f l r = combineLoc l r (cons l r)

    -- An operator of the given precedence level or higher.
    opExpr [] = fylkissegð -- would already do this, but this makes it clearer.
    opExpr levels = fylkissegð >>= opTail levels

    -- opTail is given the head of the expression, such that association is done
    -- by the parser itself.
    opTail [] hd = return hd
    opTail levels@(level:higherLevels) hd = do
        -- Check for any higher precedence stuff on the left of operators at the
        -- current level. Those need to bind to the head of the expression first.
        -- This works the same regardless of the associativity of the current level.
        -- E.g. in "1+2 : 3+4" nested would be "1+2" if we are at the level of ":".
        --      in "1*2 + 3*4" nested would be "1*2" if we are at the level of "+"
        nested <- loc $ opTail higherLevels hd
        if isRightAssoc level
            then do
                rest <- optionMaybe $ (,) <$> loc (oneOfOps level) <*> loc (opExpr levels)
                case rest of
                    Nothing -> return $ unLoc nested
                    Just (op, expr) -> return $ OperatorS op nested expr
            else do
                ops <- many $ (,) <$> loc (oneOfOps level) <*> loc (opExpr higherLevels)
                return . unLoc $ foldl (\l (op,r) -> combineLoc l r (OperatorS op l r)) nested ops

    oneOfOps chars = try (aðgerð >>= checkOp) where 
        checkOp op | take 1 op `isInfixOf` chars = return op
                   | otherwise = empty           

    -- From what I can see in the Manual, only operators beginning with ':'
    -- are right-assocative.
    isRightAssoc = (":" `isPrefixOf`)

-- | A comma-separated list of expressions
-- XXX This deviates from grammar shown in the manual, but it fixes errors, so the manual
-- is probably wrong. An example:
--     stofn
--         skrifastreng(;"Halló, "),
--         skrifastreng(;nafn),
--     stofnlok
-- The second skrifastreng should not be followed by a comma, but the Reykjavík compiler
-- accepts this syntax. The invalid [1,2,3,4,5,6,7,8,] is also accepted by the Reykjavík
-- compiler, which is why this extra comma is part of segðaruna, not smásegð or stofns.
segðaruna :: P [Located Syntax]
segðaruna = loc segð `sepEndBy` comma

-- | This pretty much corresponds to anything you can put after \\haus etc.
-- I kind of expected to need "try" here. Apparently not though.
smásegð :: P Syntax
smásegð =  LiteralS <$> tala
       <|> nafnsegð
       <|> operator
       <|> efsegð
       <|> (ListS <$> squareBrackets segðaruna <?> "list literal")
       <|> lykkjusegð
       <|> stofns (BlockS <$> segðaruna)
       <|> valsegð
       <|> BreakS <$ reserved "út"
       <|> ReturnS <$ reserved "skila" <*> loc segð
       <|> brackets segð
       <|> LiteralS <$> StringLit <$> strengur
       <|> funcRef
       <?> "expression"
    where
        operator = do
            op <- loc aðgerð
            -- Minor ambiguity here.
            -- A '(' could start a parenthesised Expr or an OperatorCallExpr.
            -- This requires two tokens of lookahead.
            try (opCall op) <|> opUnary op

        opUnary op = OperatorUnaryS op <$> loc smásegð <?> "expression for " ++ showIdent (unLoc op)

        opCall op = brackets $ do
            -- Obviously, you can't use inout parameters with an operator.
            semi <?> "\";\" before arguments passed to " ++ showIdent (unLoc op)
            args <- segðaruna <?> "arguments passed to " ++ showIdent (unLoc op)
            return $ OperatorCallS op args

        funcRef = do
            reserved "stef"
            -- XXX According to the manual, operator names are not allowed here.
            --     However, the Rejkjavík compiler accepts operators too.
            name <- loc $ nafn <|> aðgerð <?> "function or operator name"
            arity <- loc . brackets $ do
                -- XXX According to the grammar, we should be using `tala' here, but why in
                -- the fuck would anyone use a floating point or negative number here?
                -- The Reykjavík compiler parses many things here, but raises an error if it's not
                -- a fjöldatala.
                io <- fjöldatala <?> "inout arity of " ++ showIdent (unLoc name)
                semi <?> "\";\" between number of inout and in parameters for" ++ showIdent (unLoc name)
                i <- fjöldatala <?> "inward arity of " ++ showIdent (unLoc name)
                return (io, i)
            return $ FunRefS name arity

lykkjusegð :: P Syntax
lykkjusegð = meðan <|> fyrir <|> (LoopS <$> loopBody "loop body")
    where
        meðan = do
            reserved "meðan"
            WhileLoopS <$> loc segð <*> loopBody "body of while loop"
        fyrir = do
            reserved "fyrir"
            (init, cond, inc) <- brackets $ do
                init <- segðaruna <?> "initializer expression of for loop"
                cond <- semi *> (loc segð <?> "test condition of for loop")
                inc <- semi *> (segðaruna <?> "step expression of for loop")
                return (init, cond, inc)
            body <- loopBody "body of for loop"
            return $ ForLoopS init cond inc body
        loopBody lbl = between (reserved "lykkja") (reserved "lykkjulok") $ segðaruna <?> lbl

nafnsegð :: P Syntax
nafnsegð = do
    name <- loc nafn
    choice [ AssignS name <$> assign name
           , uncurry (FunCallS name) <$> apply name
           , return (VarRefS name) ]
    where
        assign name = reservedOp ":=" *> (loc segð <?> "expression to assign to " ++ showIdent (unLoc name))
        apply name = brackets $ do
            refs <- segðaruna <?> "inout arguments to " ++ showIdent (unLoc name)
            semi <?> "\";\" between inout and in arguments to " ++ showIdent (unLoc name)
            args <- segðaruna <?> "inward arguments to " ++ showIdent (unLoc name)
            return (refs, args)

efsegð :: P Syntax
efsegð = do
    cond <- reserved "ef" *> (loc segð <?> "condition of an if expression")
    then' <- reserved "þá" *> segðaruna
    elseifs <- many annarsef
    else' <- optionMaybe annars <* reserved "eflok"
    return $ IfS cond then' elseifs else'
    <?> "if expression"
    where
        annarsef = liftA2 (,) (reserved "annarsef" *> (loc segð <?> "test expression")) (reserved "þá" *> segðaruna)
        annars = reserved "annars" *> (segðaruna <?> "else clause")

valsegð :: P Syntax
valsegð = do
    value <- between (reserved "val") (reserved "úr") (loc segð <?> "scrutinee expression of case statement")
    cases <- many kostur
    def <- optionMaybe (reserved "annars" *> segðaruna) <* reserved "vallok"
    return $ CaseS value cases def
    <?> "case statement"
    where
        kostur = do
            ranges <- many1 . loc $ do
                a <- reserved "kostur" *> valfasti
                b <- optionMaybe (symbol ".." *> valfasti)
                return (a, b)
            action <- reserved "þá" *> segðaruna
            return (ranges, action)

valfasti :: P Literal
valfasti = (CharLit <$> staffasti) <|> (NatLit <$> fjöldatala)

fylkissegð :: P Syntax
fylkissegð = do
    expr <- loc smásegð
    getter <- optionMaybe $ do
        indices <- squareBrackets $ segðaruna <?> "index expressions"
        value <- optionMaybe $ reservedOp ":=" *> loc segð
        return (indices, value)
    return $ case getter of
        Nothing -> unLoc expr
        Just (indices, Nothing) -> GetterS expr indices
        Just (indices, Just value) -> SetterS expr indices value

tala :: P Literal
tala =  CharLit <$> staffasti
    <|> FloatLit <$> try fleytitala
    <|> NatLit <$> fjöldatala
    <|> IntLit <$> heiltala
    <?> "number"

--- Token parser -------------------------------------------------------------------------

fjölnirDef :: T.LanguageDef ()
fjölnirDef = T.emptyDef
           { T.commentLine = ";;"
           , T.identStart = nafnstafur
           , T.identLetter = nafnstafur <|> tölustafur
           , T.opStart = aðgerðarstafur
           , T.opLetter = aðgerðarstafur
           , T.reservedOpNames = Language.reservedOperators
           , T.reservedNames = Language.reservedNames
           , T.caseSensitive = True
           }

-- `show x' escapes icelandic characters, which is unwanted.
-- There can't be bad characters in a name, anyway.
showIdent :: String -> String
showIdent x = "\"" ++ x ++ "\""

lexer :: T.TokenParser ()
lexer = T.makeTokenParser fjölnirDef

-- I'm English, sorry.
brackets, squareBrackets, braces :: P a -> P a
brackets = T.parens lexer
squareBrackets = T.squares lexer
braces = T.braces lexer

stofns :: P a -> P a
stofns = between (reserved "stofn") (reserved "stofnlok")

semi, comma :: P String
semi = T.semi lexer
comma = T.comma lexer

reserved, reservedOp, symbol :: String -> P String
reserved x = x <$ (T.reserved lexer x <?> showIdent x)
reservedOp x = x <$ T.reservedOp lexer x
symbol = T.symbol lexer

lexeme :: P a -> P a
lexeme = T.lexeme lexer

whiteSpace :: P ()
whiteSpace = T.whiteSpace lexer

-- Skip initial whitespace, and require EOF (whitespace is consumed by the
-- lexeme parsers) at the end.
parseGeneric :: P a -> SourceName -> String -> Either ParseError a
parseGeneric p name text =
    parse (whiteSpace *> p <* eof) name text

parseProgram :: SourceName -> String -> Either ParseError Program
parseProgram = parseGeneric forrit

parseExpression :: SourceName -> String -> Either ParseError (Located Syntax)
parseExpression = parseGeneric (loc segð)

parseExpressions :: SourceName -> String -> Either ParseError [Located Syntax]
parseExpressions = parseGeneric segðaruna

parseModule :: SourceName -> String -> Either ParseError (Located ModuleDecl)
parseModule = parseGeneric (loc eining)

parseInnflutt :: SourceName -> String -> Either ParseError [Located Name]
parseInnflutt = parseGeneric innflutt

parseStaðvær :: SourceName -> String -> Either ParseError [(Located Name, Maybe (Located Syntax))]
parseStaðvær = parseGeneric staðvær
