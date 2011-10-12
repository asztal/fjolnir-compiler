{-# LANGUAGE RecordWildCards #-}

module ExpFromAST
    ( syntaxToExp
    , compileFunction
    , Function (..)
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad (msum)
import Data.Array.Unboxed (listArray)
import Data.Char (ord)
import Data.Either (partitionEithers)
import Data.List (sortBy, genericLength)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as M
import Data.Ord (comparing)

import Compiler
import Exp
import AST
import Var
    
syntaxToExp :: Map LVarName Int 
            -> Map LVarName Int
            -> Map LVarName Int 
            -> [LVarName]
            -> Located Syntax
            -> Compiler (Exp Var Fun)
syntaxToExp localIndices argIndices refArgIndices importedVars (L loc expr) =
    context $ case expr of
        LiteralS lit -> return $ transformLiteral lit
        ListS [] -> return NilE
        ListS (x:xs) -> apply (withLoc x ":") [] [x, L loc $ ListS xs] -- Location info isn't perfect here.

        FunRefS name (L _ arity) -> return $ FunE (ImportedFun name arity) arity
        VarRefS name -> ReadE <$> lookupVarD name

        GetterS a is -> recur . L loc $ OperatorCallS (withLoc a $ "fylkissækja" ++ show (length is)) (a:is)
        SetterS a is e -> recur . L loc $ OperatorCallS (withLoc a $ "fylkissetja" ++ show (length is)) (a:is ++ [e])
        AssignS name e -> WriteE <$> lookupVarD name <*> recur e

        FunCallS name refs args -> apply name refs args
        OperatorS name x y -> apply name [] [x, y]
        OperatorCallS name args -> apply name [] args
        OperatorUnaryS name x -> apply name [] [x]

        AndS x y -> AndE <$> recur x <*> recur y
        OrS x y -> OrE <$> recur x <*> recur y
        NotS x -> NotE <$> recur x

        CaseS cond cases defaultCase -> do
            CaseE
            <$> recur cond
            <*> mapM toBranch cases
            <*> maybe (pure NilE) recurs defaultCase

        IfS condition thenExp elseIfs otherwiseExp ->
            fold $ (condition, thenExp) : elseIfs
            where
                fold [] = maybe (pure NilE) recurs otherwiseExp
                fold ((cond, body) : rest) = 
                    IfE <$> recur cond <*> recurs body <*> fold rest

        WhileLoopS cond body -> while cond body
        ForLoopS inits cond incs body -> for inits cond incs body 
        
        LoopS xs -> LoopE . ManyE <$> mapM recur xs

        BlockS [] -> return NilE
        BlockS [x] -> recur x
        BlockS xs -> ManyE <$> mapM recur xs
        
        BreakS -> return BreakE
        ReturnS x -> ReturnE <$> recur x
    
    where
        context = withErrorContext $ "When compiling function at " ++ show loc
    
        recur = syntaxToExp localIndices argIndices refArgIndices importedVars

        recurs [] = return NilE
        recurs [x] = recur x
        recurs xs = ManyE <$> mapM recur xs

        while cond body = do
            breakExp <- IfE <$> recur cond <*> pure NilE <*> pure BreakE
            LoopE . insertBreak breakExp <$> recurs body
            where
                insertBreak e (ManyE xs) = ManyE (e : xs)
                insertBreak e NilE = e
                insertBreak e x = ManyE [e, x]

        for inits cond incs body = do
            inits' <- recurs inits
            body' <- while cond (body ++ incs)
            return . simplifyE $ ManyE [inits', body']

        simplifyE (ManyE xs) = ManyE $ concatMap f xs where
            f (ManyE ys) = ys
            f x = [x]
        simplifyE e = e
        
        apply name refs args = AppE
            <$> pure (possiblyFun name (genericLength refs, genericLength args))
            <*> mapM toRefVar refs
            <*> mapM recur args

        toRefVar (L l e) = case e of
            VarRefS name -> Right <$> lookupVarD name
            x -> Left <$> recur (L l x)

        lookupVarD name = 
            maybe (tryImport name) return $ lookupVar name

        tryImport name
            | name `elem` importedVars = return $ ImportedVar name
            | otherwise = compileError (getLoc name) [ "Unbound variable " ++ show name ++ " used (did you mean to import it?)" ]

        lookupVar name = msum $ zipWith find
                [localIndices, argIndices, refArgIndices]
                [LocalVar, ArgVar, RefArgVar]
            where
                find vars constr = constr <$> M.lookup name vars

        possiblyFun name arity = maybe (Right (ImportedFun name arity)) Left $ lookupVar name

        transformLiteral (CharLit c) = WordE $ fromIntegral (ord c)
        transformLiteral (NatLit i) = WordE $ fromIntegral i
        transformLiteral (IntLit i) = WordE $ fromIntegral i
        transformLiteral (FloatLit f) = RealE f
        transformLiteral (StringLit xs) = StrE $ let len = length xs in
            listArray (0, len) (fromIntegral len : map (fromIntegral . ord) xs)

        toBranch (ranges, body) = do
            bodyExp <- recurs body
            return (map toRange ranges, bodyExp)
            
        toRange (L _ (from, to)) =
            (literalAsWord16 from, literalAsWord16 $ fromMaybe from to)

        literalAsWord16 (CharLit c) = fromIntegral (ord c)
        literalAsWord16 (NatLit i) = fromIntegral i
        literalAsWord16 _ = error "Literal in case range wasn't CharLit/NatLit"

data Function = Function
    { fnArity :: Arity
    , fnLocalCount :: Int
    , fnBody :: Exp Var Fun
    }

compileFunction :: Located FunctionDecl -> Compiler Function
compileFunction (L funLoc FunctionDecl {..}) = context $ do
    checkVarNames $ fnInOutParams ++ fnInParams ++ concatMap variableDeclNames fnVariables

    body <- compileBody
    
    return $ Function
        (genericLength fnInOutParams, genericLength fnInParams)
        (length allLocals)
        body

    where
        context = withErrorContext $ "When compiling function at " ++ show funLoc
    
        compileBody = do
            bodyExps <- mapM toExp fnBody
            initExps <- compileInitializers
            return $ simplifyE (ManyE (initExps ++ bodyExps))

        simplifyE (ManyE []) = NilE
        simplifyE (ManyE [x]) = x
        simplifyE x = x

        -- Convert the initializer list to a list of statements to be inserted
        -- before the function body.
        compileInitializers = catMaybes <$> (sequence $ zipWith f [0..] allLocals) where
            f i (_, Just initializer) = Just . WriteE (LocalVar i) <$> toExp initializer
            f _ (_, Nothing) = return Nothing

        (allLocals, importedVars) = (concat *** concat) . partitionEithers $ map f fnVariables where
            f (LocalVarDecl xs) = Left xs
            f (ImportVarDecl xs) = Right xs

        checkVarNames xs = checkNameClashes funLoc "Variable declared multiple times: " xs

        argMap = indexMap fnInParams
        refArgMap = indexMap fnInOutParams
        localMap = indexMap locals

        indexMap = M.fromList . sortBy (comparing snd) . (`zip` [0..])

        (locals, _) = partitionEithers $ concatMap f fnVariables where
            f (LocalVarDecl xs) = map (Left . fst) xs
            f (ImportVarDecl names) = map Right names

        toExp = syntaxToExp localMap argMap refArgMap importedVars

