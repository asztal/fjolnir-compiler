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

-- Imports -------------------------------------------------------------

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
moduleImports (CM _ x) =
    M.unionsWith mappend <$> mapM (uncurry exportImports) (M.toList x)
    where
        exportImports :: Name -> CompiledExport -> Compiler (Map Name ImportType)
        exportImports n (FunCE funID) = do
            CompiledFunction _ _ ir <- retrieveFunction funID 
            return (irImports ir)
        exportImports n (ReCE (L _ name)) = return $ M.singleton name ImportAny
        exportImports n _ = return M.empty
        
checkImportsValid :: CompiledModule -> Compiler ()
checkImportsValid m@(CM loc _) = do
    imports <- moduleImports m
    let errors = M.filter isImportError imports
    when (not (M.null errors)) $ do
        moduleError m $
            "Conflicting imports found in a module:"
            : [ "  " ++ name ++ ": " ++ show err | (name, err) <- M.toList errors ]
        
    where
        isImportError (ImportError _) = True
        isImportError _ = False

-- Compiling -----------------------------------------------------------

compileCodeModule :: SrcSpan -> Module -> Compiler CompiledModule
compileCodeModule loc (Module exports) = do
    m <- CM (sourceModule loc) <$> T.forM exports compile
    checkImportsValid m
    return m
    where 
        compile (FunExport decl) = do
            Function {..} <- compileFunction (noLoc decl)
            FunCE <$> defineFunction 
                (CompiledFunction fnArity fnLocalCount (compileToIR fnBody))
        compile (VarExport) = VarCE <$> newVarID
        compile (ReExport name) = return $ ReCE name
        compile (NativeExport nf) = return $ NativeCE nf

compose, combine, andModule :: SrcSpan -> CompiledModule -> CompiledModule -> Compiler CompiledModule 
plus :: SrcSpan -> Bool -> CompiledModule -> CompiledModule -> Compiler CompiledModule 

plus loc ignoreClashes (CM _ x) (CM _ y) = 
    let clashes = M.intersectionWith (,) x y
    in if M.null clashes || ignoreClashes
        then return $ CM (sourceModule loc) $ M.union x y
        else do 
            let errLines = [ " * " ++ name ++ " (" ++ showType x' ++ " and " ++ showType y' ++ ")" 
                           | (name, (x', y')) <- M.toList clashes ] 
                showType (FunCE _) = "stef"
                showType (NativeCE _) = "cstef"
                showType (VarCE _) = "breyta"
                showType (ReCE (L _ name)) = "re-export of " ++ show name 
            compileError noSpan $ 
                "The following names were exported from both sides of a module sum:"
                : errLines
             
compose loc (CM _ x) (CM _ y) = CM (sourceModule loc) <$> T.mapM (resolveImportsWith y) x
combine loc x y = (flip (plus loc True) y) =<< compose loc x y
andModule loc x y = iterateModule loc =<< plus loc False x y

iterateModule :: SrcSpan -> CompiledModule -> Compiler CompiledModule
iterateModule loc (CM _ x) = context $ CM (sourceModule loc) <$> T.forM x resolve
    where
        context = withErrorContext $ "When iterating the module at " ++ show loc
    
        resolve (FunCE funID) = FunCE <$> resolveFunWith x funID
        resolve r@(ReCE (L loc name)) = resolveChain x [] r
        resolve x = return x
        
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
        Just (NativeCE (NativeFunction cName libName _)) -> compileError loc ["Resolving name '" ++ name ++ "': expected a variable, found a native function (" ++ libName ++ "." ++ cName ++ ")"]
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
        Just (NativeCE nf) -> return $ ResolvedNativeFun nf
    ff r = return r

resolveImportsWith :: Map Name CompiledExport -> CompiledExport -> Compiler CompiledExport
resolveImportsWith src (FunCE funID) = do
    CompiledFunction arity locals ir <- retrieveFunction funID
    ir' <- resolveIR src ir
    FunCE <$> defineFunction
        (CompiledFunction arity locals ir')
resolveImportsWith src (ReCE (L loc name)) = return $ case M.lookup name src of
    Just e -> e
    Nothing -> ReCE (L loc name)
resolveImportsWith src x = return x

compileModule :: ModuleExpr -> Compiler CompiledModule
compileModule (CodeM loc m) = compileCodeModule loc m
compileModule (GlobalM name) = globalModule name
compileModule (VarM name) = moduleVariable name
compileModule (PlusM loc a b) = withErrorContext
    ("when compiling the module sum " ++ show loc)
    $ join $ plus loc False <$> compileModule a <*> compileModule b
compileModule (ComposeM loc a b) = withErrorContext
    ("when composing the modules around the '*' at " ++ show loc)
    $ join $ compose loc <$> compileModule a <*> compileModule b
compileModule (CombineM loc a b) = withErrorContext
    ("when combining the modules around the ':' at " ++ show loc)
    $ join $ combine loc <$> compileModule a <*> compileModule b
compileModule (AndM loc a b) = withErrorContext
    ("when combining the modules around the '&' at " ++ show loc)
    $ join $ andModule loc <$> compileModule a <*> compileModule b
compileModule (IterateM loc a) = withErrorContext 
    ("when iterating the module at " ++ show loc)
    $ iterateModule loc =<< compileModule a

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
        [] -> return . CodeM loc . Module . M.fromList . map (first unLoc) $ exprs
        xs -> compileError loc $
            "Code module exports names more than once"
            : map showDuplicate (concat duplicates)
    where
        f :: ExportDecl -> Export
        f (ExportAgainDecl name) = ReExport name
        f (ExportFunDecl (L _ decl)) = FunExport decl
        f ExportVarDecl = VarExport
        f (ExportNative (L _ cName) (L _ libName) (L _ arity)) 
            = NativeExport (NativeFunction cName libName arity)
exprFromDecl (GlobalModule name) = return $ GlobalM (unLoc name)
exprFromDecl (ModuleVariable name) = return $ VarM (unLoc name)
exprFromDecl (RecursiveModule (L loc decl)) = IterateM loc <$> exprFromDecl decl
exprFromDecl (CombinedModule (L _ a) (L loc name) (L _ b)) = do
    a' <- exprFromDecl a
    b' <- exprFromDecl b
    case name of
        "+" -> return $ PlusM loc a' b'
        "*" -> return $ ComposeM loc a' b'
        ":" -> return $ CombineM loc a' b'
        "&" -> return $ AndM loc a' b'

data ModuleExpr
    = CodeM SrcSpan Module -- { }
    | GlobalM Name -- "EINING"
    | VarM Name    -- ein
    | PlusM SrcSpan ModuleExpr ModuleExpr      -- a + b
    | CombineM SrcSpan ModuleExpr ModuleExpr   -- a : b
    | ComposeM SrcSpan ModuleExpr ModuleExpr   -- a * b
    | IterateM SrcSpan ModuleExpr              -- !a
    | AndM SrcSpan ModuleExpr ModuleExpr       -- a & b
    
    -- No need for case to represent the "&" operator.
    -- a & b is equivalent to !(a + b).
