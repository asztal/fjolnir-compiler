{-# LANGUAGE Rank2Types, GADTs, TypeSynonymInstances, DeriveDataTypeable,
             RecordWildCards #-}

module AST
    ( -- * Syntax Tree
      Literal (..)
    , Syntax (..)
    , VariableDecl (..)
    , FunctionDecl (..)
    , ExportDecl (..)
    , ModuleDecl (..)
    , Program
    , ProgramStatement (..)
      -- * Types
    , module Types
      -- * AST operations
    , variableDeclNames
    , functionImports
    , deepModifyAST
    , subexpressions
    ) where

import Data.Char (isAlpha)
import Data.Either (partitionEithers)
import Data.List (intercalate, isPrefixOf, isInfixOf, findIndex)
import Data.Generics (Data, Typeable, everywhere, mkT, listify)
import Text.Show

import Language
import Located
import Types

-- | A value literal of type Char (stafur), Natural number (Fjöldatala), Integer (Heiltala)
-- | Floating point number (Fleytitala), or String (Strengur).
data Literal where
    CharLit   :: Char    -> Literal
    NatLit    :: Integer -> Literal
    IntLit    :: Integer -> Literal
    FloatLit  :: Double  -> Literal
    StringLit :: String  -> Literal
    deriving (Data, Typeable)

-- TODO: Use Fjölnir escaping rules for CharLit/StringLit instead of Haskell's.
instance Show Literal where
    show (CharLit c) = show c
    show (StringLit str) = show str
    show (NatLit n) = show n
    show (IntLit n) = show n
    show (FloatLit f) = show f

type LSyntax = Located Syntax
type Block = [LSyntax]

data Syntax
    = LiteralS Literal
    | ListS [LSyntax]
    
    | FunRefS LFunName (Located Arity)    -- stef f(0,2)
    | VarRefS LVarName                    -- Reference to a variable

    | GetterS LSyntax [LSyntax]
    | SetterS LSyntax [LSyntax] LSyntax
    | AssignS LVarName LSyntax

    | FunCallS LFunName [LSyntax] [LSyntax]  -- f(inout;in)
    | OperatorS LFunName LSyntax LSyntax     -- x ++ y OR x \fgcd y
    | OperatorCallS LFunName [LSyntax]       -- \hali(;xs)
    | OperatorUnaryS LFunName LSyntax        -- \hali xs

    -- ekki, og, and eða are treated separately from normal operators.
    -- AFAICS it is not possible to redefine their behaviour.
    -- This is most likely due to short-circuiting behaviour.
    | AndS LSyntax LSyntax
    | OrS LSyntax LSyntax
    | NotS LSyntax

    | CaseS LSyntax [([Located (Literal, Maybe Literal)], Block)] (Maybe Block)
    | IfS LSyntax Block [(LSyntax, Block)] (Maybe Block)

    -- meðan cond lykkja body lykkjulok
    | WhileLoopS LSyntax Block                 
    -- fyrir (initializers; condition; increments) lykkja body lykkjulok
    | ForLoopS [LSyntax] LSyntax [LSyntax] Block
    -- lykkja body lykkjulok
    | LoopS Block

    | BlockS Block
    | BreakS
    | ReturnS LSyntax
    deriving (Data, Typeable)

-- Corresponds to a single staðvær or innflutt statement.
data VariableDecl
    = LocalVarDecl [(LVarName, Maybe LSyntax)]
    | ImportVarDecl [LVarName]
    deriving Show

variableDeclNames :: VariableDecl -> [LVarName]
variableDeclNames (LocalVarDecl xs) = map fst xs
variableDeclNames (ImportVarDecl xs) = xs

data FunctionDecl = FunctionDecl
    { fnInOutParams :: [LVarName]
    , fnInParams :: [LVarName]
    , fnVariables :: [VariableDecl]
    , fnBody :: Block
    } deriving Show

data ExportDecl
    = ExportAgainDecl (LName)
    | ExportVarDecl
    | ExportFunDecl (Located FunctionDecl)
    deriving Show

data ModuleDecl
    = ModuleDecl (Located [(LName, ExportDecl)])
    | GlobalModule LName
    | ModuleVariable LName
    | RecursiveModule (Located ModuleDecl)
    | CombinedModule (Located ModuleDecl) LName (Located ModuleDecl)
    deriving Show

type Program = [Located ProgramStatement]

data ProgramStatement
    = DefineGlobalModule LName (Located ModuleDecl)
    | DefineModuleVariable LName (Located ModuleDecl)
    | DefineEntryPoint LName LFunName (Located ModuleDecl)
    deriving Show

deepModifyAST :: (Syntax -> Syntax) -> (Syntax -> Syntax)
deepModifyAST f = everywhere (mkT f)

functionImports :: FunctionDecl -> ([LVarName], [LFunName])
functionImports FunctionDecl {..} =
    partitionEithers . concatMap f . concatMap subexpressions $ map unLoc fnBody
    where
        f (FunRefS n _) = fun n
        f (VarRefS n) = var n
        f (AssignS n _) = var n
        f (FunCallS n _ _) = fun n 
        f (OperatorS n _ _) = fun n
        f (OperatorCallS n _) = fun n
        f (OperatorUnaryS n _) = fun n
        f _ = []
        filterLocals constr n
            | n `elem` localNames = []
            | otherwise = [constr n]
        fun = filterLocals Right
        var = filterLocals Left
        localNames = 
            fnInOutParams ++
            fnInParams ++
            concatMap variableDeclNames fnVariables

subexpressions :: Syntax -> [Syntax]
subexpressions = listify (const True)

-- Sadly, this can't be automated.

prepend :: [a] -> [a] -> [a]
prepend x y = x ++ y

showsCommaSep :: Show a => [Located a] -> String -> String
showsCommaSep [] = prepend ""
showsCommaSep [L _ x] = shows x
showsCommaSep (L _ x : xs) = shows x . prepend ", " . showsCommaSep xs

instance Show Syntax where
    showList xs = prepend $ intercalate "," (map show xs)

    -- All of these don't care what the current precendence context is.
    showsPrec _ (LiteralS lit)
        = prepend (show lit)

    showsPrec _ (FunRefS (L _ n) (L _ (io, i)))
        = prepend "stef " . prepend n . showParen True (shows io . prepend ";" . shows i)

    showsPrec _ (VarRefS (L _ n))
        = prepend n

    showsPrec _ (ListS xs)
        = showListWith shows (map unLoc xs)

    showsPrec _ BreakS
        = prepend "út"

    showsPrec _ (LoopS xs)
        = prepend "lykkja " . showsCommaSep xs . prepend " lykkjulok"

    showsPrec _ (ForLoopS inits (L _ test) incs body)
        = prepend "fyrir("
        . showsCommaSep inits
        . prepend ";"
        . shows test
        . prepend ";"
        . showsCommaSep incs
        . prepend ") "
        . shows (LoopS body)

    showsPrec _ (WhileLoopS (L _ x) body)
        = prepend "meðan " . shows x . shows (LoopS body)

    showsPrec p (OrS (L _ x) (L _ y))
        = showParen (p > 1) (showsPrec 1 x . prepend " eða " . showsPrec 1 y)

    showsPrec p (AndS (L _ x) (L _ y))
        = showParen (p > 2) (showsPrec 2 x . prepend " og " . showsPrec 2 y)

    showsPrec p (NotS (L _ x))
        = showParen (p > 3) (prepend "ekki " . showsPrec 3 x)

    showsPrec p (GetterS (L _ x) xs)
        = showParen (p > 4) $ showsPrec 4 x . showListWith shows (map unLoc xs)

    showsPrec p (SetterS (L _ x) xs (L _ y))
        = showParen (p > 4) $ showsPrec 4 x . showListWith shows (map unLoc xs) . prepend ":=" . shows y

    showsPrec p (AssignS (L _ n) (L _ x))
        = showParen (p > 4) $ prepend (n ++ " := ") . shows x

    showsPrec p (ReturnS (L _ x))
        = showParen (p > 4) $ prepend "skila " . shows x

    showsPrec p (CaseS (L _ x) cases y)
        = showParen (p > 4) $ prepend "val " . shows x . prepend " úr " . showCases cases . showDefault y . prepend " vallok"
        where
            showCases [] = prepend ""
            showCases (z:zs) = showCase z . showCases zs
            showCase (ranges, exprs) = prepend "kostur " . showRanges ranges . prepend " þá " . showList exprs
            showRanges [] = prepend ""
            showRanges (L _ z : zs) = showRange z . showRanges zs
            showRange (from, Just to) = shows from . prepend ".." . shows to
            showRange (from, Nothing) = shows from
            showDefault (Just xs) = prepend " annars " . showsCommaSep xs
            showDefault Nothing = prepend ""

    showsPrec p (IfS (L _ x) ts cases def)
        = showParen (p > 4) $ prepend "ef " . shows x . prepend " þá " . showsCommaSep ts . showCases cases . showDef def . prepend " eflok"
        where
            showCases [] = prepend ""
            showCases (y:ys) = showCase y . showCases ys
            showCase (L _ c, t) = prepend " annarsef " . shows c . prepend " þá " . showsCommaSep t
            showDef (Just xs) = prepend " annars " . showsCommaSep xs
            showDef Nothing = prepend ""

    showsPrec _ (BlockS xs)
        = prepend "stofn " . showsCommaSep xs . prepend " stofnlok"

    showsPrec p (OperatorUnaryS (L _ n) (L _ x))
        = showParen (p > 4) $ prepend "\\" . prepend n . prepend " " . showsPrec 4 x

    showsPrec p (OperatorCallS (L _ n) xs)
        = showParen (p > 4) $ prepend n . prepend "(;" . showsCommaSep xs . prepend ")"

    showsPrec p (OperatorS (L _ o) (L _ x) (L _ y))
        = showParen (p > p') $ showsPrec pl x . prepend " " . prepend o' . prepend " " . showsPrec pr y
        where
            p' = opPrec o
            (pl, pr) = if ":" `isPrefixOf` o
                then (p' + 1, p')
                else (p', p' + 1)
            o' = if isAlpha (head o)
                then '\\' : o
                else o

    showsPrec _ (FunCallS (L _ n) refs args)
        = prepend n . prepend "(" . showsCommaSep refs . prepend ";" . showsCommaSep args . prepend ")"

opPrec :: Name -> Int
opPrec oper = case findIndex (take 1 oper `isInfixOf`) precedences of
    Nothing -> 5
    Just p -> 5 + p
