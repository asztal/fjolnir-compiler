{-# LANGUAGE RecordWildCards #-}

module Module (
    performProgramStatement
    ) where

import Control.Applicative
import Control.Arrow ((***), first)
import Control.Monad (join, when, forM_)
import Data.Function (on)
import Data.List (groupBy, sort, sortBy, union, intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Ord (comparing)
import qualified Data.Traversable as T

import AST
import Compiler
import ExpFromAST
import IR
import Located
import Var
import Types

-- TODO Add location info to modules
-- type ModuleInfo = Either SrcSpan ModuleName {- global -}
-- data CompiledModule = CompiledModule ModuleInfo (Map LName CompiledExport)
--      Use LName for key type? (Located defines appropriate Eq and Ord)

data ImportType
    = ImportFun Arity
    | ImportVar
    | ImportAny
    | ImportError [ImportType]
    deriving (Eq, Ord)
    
instance Show ImportType where
    show (ImportFun (r,v)) = "[stef " ++ show r ++ " " ++ show v ++ "]"
    show ImportVar = "breyta"
    show ImportAny = "*"
    show (ImportError xs) = intercalate " and " (map show (sort xs)) 

instance Monoid ImportType where
    mempty = ImportAny
    
    mappend ImportAny x = x
    mappend x ImportAny = x
    mappend (ImportError xs) (ImportError ys)
        = ImportError (xs `union` ys)
    mappend (ImportError xs) x
        | x `elem` xs = ImportError xs
        | otherwise = ImportError (x : xs)
    mappend x (ImportError xs)
        | x `elem` xs = ImportError xs
        | otherwise = ImportError (x : xs)
    mappend x y 
        | x == y = x
        | otherwise = ImportError [x,y]

irImports :: [Instruction Int] -> Map Name ImportType
irImports = M.unionsWith mappend . map f where
    f instr = M.fromListWith mappend . concat $ 
        map g (varRefs instr) ++ map h (funRefs instr)
    
    g (ImportedVar (L _ name)) = [(name,ImportVar)]
    g _ = []
    
    h (ImportedFun (L _ name) arity) = [(name,ImportFun arity)]
    h _ = []

moduleImports :: CompiledModule -> Compiler (Map Name ImportType)
moduleImports (CM x) =
    M.unionsWith mappend <$> mapM (uncurry exportImports) (M.toList x)
    where
        exportImports :: Name -> CompiledExport -> Compiler (Map Name ImportType)
        exportImports n (FunCE funID) = do
            CompiledFunction _ _ ir <- retrieveFunction funID 
            return (irImports ir)
        exportImports n (VarCE _) = return M.empty
        exportImports n (ReCE (L _ name)) = return $ M.singleton name ImportAny
        
checkImportsValid :: CompiledModule -> Compiler ()
checkImportsValid m = do
    imports <- moduleImports m
    let errors = M.filter isImportError imports
    when (not (M.null errors)) $ do
        compileError noSpan $
            "Conflicting imports found in a module:"
            : [ "  " ++ name ++ ": " ++ show err | (name, err) <- M.toList errors ]
        
    where
        isImportError (ImportError _) = True
        isImportError _ = False

compileCodeModule :: Module -> Compiler CompiledModule
compileCodeModule (Module exports) = do
    m <- CM <$> T.forM exports compile
    checkImportsValid m
    return m
    where 
        compile (FunExport decl) = do
            Function {..} <- compileFunction (noLoc decl)
            FunCE <$> defineFunction 
                (CompiledFunction fnArity fnLocalCount (compileToIR fnBody))
        compile (VarExport) = VarCE <$> newVarID
        compile (ReExport name) = return $ ReCE name

-- TODO '+' should error if both modules define the same export
plus, compose, combine :: CompiledModule -> CompiledModule -> Compiler CompiledModule 
plus (CM x) (CM y) = return $ CM $ M.union x y
compose (CM x) (CM y) = CM <$> T.mapM (resolveImportsWith y) x
combine x y = (`plus` y) =<< compose x y

iterateModule :: CompiledModule -> Compiler CompiledModule
iterateModule (CM x) = CM <$> T.forM x resolve
    where
        resolve (FunCE funID) = FunCE <$> resolveFunWith x funID
        resolve (VarCE varID) = return $ VarCE varID
        resolve r@(ReCE (L loc name)) = resolveChain x [] r
        
        resolveChain src seen (ReCE n@(L loc name))
            | name `elem` map unLoc seen = compileError loc $
                "Re-export cycle detected during module iteration: "
                : showCycle (head seen : reverse seen)
            | otherwise = case M.lookup name x of
                Just r@(ReCE n@(L loc' name')) -> 
                    resolveChain src (n : seen) r
                Just e -> return e
                Nothing -> return $ ReCE (L loc name)
        resolveChain _ _ x = return x
                
        showCycle xs = [ "  " ++ unLoc name ++ atLoc name | name <- xs ]

resolveFunWith :: Map Name CompiledExport -> FunID -> Compiler FunID
resolveFunWith src funID = do
    CompiledFunction arity locals ir <- retrieveFunction funID
    ir' <- resolveIR src ir
    defineFunction
        (CompiledFunction arity locals ir')

resolveIR :: Map Name CompiledExport -> [Instruction Int] -> Compiler [Instruction Int]
resolveIR src ir = mapM (modifyIRVarRefs vf ff) ir where
    vf r@(ImportedVar (L loc name)) = case M.lookup name src of
        Nothing -> return r
        Just (VarCE varID) -> return $ ResolvedVar varID 
        Just (FunCE _) -> compileError loc ["Resolving name '" ++ name ++ "': expected a variable, found a function"]
        Just (ReCE name') -> return $ ImportedVar name'
    vf r = return r
    
    ff r@(ImportedFun (L loc name) arity ) = case M.lookup name src of
        Nothing -> return r
        Just (FunCE funID) -> do
            CompiledFunction arity' _ _ <- retrieveFunction funID
            when (arity /= arity') $
                compileError loc [
                    "Resolving " ++ name ++ ": expected a function of arity " ++ show arity 
                    ++ "; found  a function of arity " ++ show arity']
            return $ ResolvedFun funID
        Just (VarCE _) -> compileError loc ["Resolving name '" ++ name ++ "': expected a function, found a variable"]
        Just (ReCE name') -> return $ ImportedFun name' arity
    ff r = return r

resolveImportsWith :: Map Name CompiledExport -> CompiledExport -> Compiler CompiledExport
resolveImportsWith src (FunCE funID) = do
    CompiledFunction arity locals ir <- retrieveFunction funID
    ir' <- resolveIR src ir
    FunCE <$> defineFunction
        (CompiledFunction arity locals ir')
resolveImportsWith src (VarCE varID) = return $ VarCE varID
resolveImportsWith src (ReCE (L loc name)) = return $ case M.lookup name src of
    Just e -> e
    Nothing -> ReCE (L loc name)

compileModule :: ModuleExpr -> Compiler CompiledModule
compileModule (CodeM m) = compileCodeModule m
compileModule (GlobalM name) = globalModule name
compileModule (VarM name) = moduleVariable name
compileModule (PlusM a b) =
    join $ plus <$> compileModule a <*> compileModule b
compileModule (ComposeM a b) =
    join $ compose <$> compileModule a <*> compileModule b
compileModule (CombineM a b) =
    join $ combine <$> compileModule a <*> compileModule b
compileModule (IterateM a) =
    iterateModule =<< compileModule a

performProgramStatement :: ProgramStatement -> Compiler ()
performProgramStatement (DefineGlobalModule (L loc name) (L _ decl)) = 
    withErrorContext ("when defining module " ++ show name ++ inlineLoc loc) $ do
        cm <- compileModule =<< exprFromDecl decl
        checkImportsValid cm
        defineGlobalModule name cm
performProgramStatement (DefineModuleVariable n@(L loc name) (L _ decl)) = 
    withErrorContext ("when defining module " ++ name ++ inlineLoc loc) $ do
        cm <- compileModule =<< exprFromDecl decl
        checkImportsValid cm
        defineModuleVariable name cm
performProgramStatement (DefineEntryPoint n@(L loc name) (L _ funName) (L _ decl)) =
    withErrorContext ("when defining entry point " ++ show name ++ inlineLoc loc) $ do
        cm <- compileModule =<< exprFromDecl decl
        checkImportsValid cm
        defineEntryPoint name funName cm

exprFromDecl :: ModuleDecl -> Compiler ModuleExpr
exprFromDecl (ModuleDecl (L loc code)) = 
    let exprs = map (id *** f) code
        eq = (==) `on` (unLoc . fst)
        cmp = comparing (unLoc . fst)
        groups = groupBy eq . sortBy cmp $ exprs 
        duplicates = filter (not . null . tail) groups
        showDuplicate (name, _) = "  " ++ unLoc name ++ atLoc name
    in case duplicates of
        [] -> return . CodeM . Module . M.fromList . map (first unLoc) $ exprs
        xs -> compileError loc $
            "Code module exports names more than once"
            : map showDuplicate (concat duplicates)
    where
        f :: ExportDecl -> Export
        f (ExportAgainDecl name) = ReExport name
        f (ExportFunDecl (L _ decl)) = FunExport decl
        f ExportVarDecl = VarExport
exprFromDecl (GlobalModule name) = return $ GlobalM (unLoc name)
exprFromDecl (ModuleVariable name) = return $ VarM (unLoc name)
exprFromDecl (RecursiveModule decl) = IterateM <$> exprFromDecl (unLoc decl)
exprFromDecl (CombinedModule (L _ a) (L _ name) (L _ b)) = do
    a' <- exprFromDecl a
    b' <- exprFromDecl b
    case name of
        "+" -> return $ PlusM a' b'
        "*" -> return $ ComposeM a' b'
        ":" -> return $ CombineM a' b'
        "&" -> return $ IterateM (PlusM a' b')

data ModuleExpr
    = CodeM Module -- { }
    | GlobalM Name -- "EINING"
    | VarM Name    -- ein
    | PlusM ModuleExpr ModuleExpr      -- a + b
    | CombineM ModuleExpr ModuleExpr   -- a : b
    | ComposeM ModuleExpr ModuleExpr   -- a * b
    | IterateM ModuleExpr              -- !a
    
    -- No need for case to represent the "&" operator.
    -- a & b is equivalent to !(a + b).
