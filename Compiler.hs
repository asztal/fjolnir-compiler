{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts,
             RecordWildCards, TemplateHaskell #-}

module Compiler
    ( Compiler, CompilerError, runCompiler
    , compileError, panic, withErrorContext
    , debugMessage
    , checkNameClashes
    , parseProgram
    
    , ModuleLoc, namedModule, sourceModule, moduleError
    , Module(..), CompiledModule(..)
    , Export(..), CompiledExport(..)
    , VarID, newVarID, CompiledFunction(..)
    , loadExternalModules 
    , defineGlobalModule
    , defineModuleVariable
    , defineEntryPoint
    , defineFunction
    , findUsedFunsAndVars
    
    , getEntryPoints
    , retrieveFunction
    , moduleVariable
    , globalModule
    
    , module Located
    ) where

import Control.Arrow (first, second)
import Control.Applicative
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Monad.Error (MonadError(..), Error(..), ErrorT, runErrorT)
import Control.Monad.State (StateT, MonadState(..), 
                            evalStateT, execStateT, modify, gets, put)
import Control.Monad

import Data.Binary (encodeFile, decodeFile)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Traversable as T

import System.FilePath (addExtension, takeBaseName, takeFileName)
import qualified System.FilePath.Find as F
import System.IO (hPutStrLn, stderr)

import AST
import DeriveBinary
import IR
import Located
import Var
import Types

import Text.Parsec.Error
import Text.Parsec.Pos
import qualified Parser (parseProgram)

-- Monad ---------------------------------------------------------------

newtype Compiler a = Compiler ((StateT CompilerState (ErrorT CompilerError IO)) a)
    deriving (Functor, Monad, MonadIO, 
              MonadError CompilerError, MonadState CompilerState)

data CompiledFunction = CompiledFunction Arity Int [Instruction Int]

data CompilerState = CompilerState
    { csNextID :: Int
    , csModuleVariables :: Map Name CompiledModule 
    , csGlobalModules :: Map Name (Maybe CompiledModule)
    , csEntryPoints :: Map Name FunID
    , csFunctions :: Map FunID CompiledFunction
    , csNewModules :: Set Name
    }
    
defaultCompilerState = CompilerState
    0
    M.empty
    M.empty
    M.empty
    M.empty
    S.empty

instance Applicative Compiler where
    pure = return
    (<*>) = ap

runCompiler :: Compiler a -> IO (Either CompilerError a)
runCompiler (Compiler x) = runErrorT (evalStateT x defaultCompilerState)

-- Errors --------------------------------------------------------------

data CompilerError = CompilerError SrcSpan [String] [ErrorContext]

type ErrorContext = String

instance Error CompilerError where
    noMsg = strMsg "Unknown error (someone called noMsg)"
    strMsg xs = CompilerError noSpan [xs] []

instance Show CompilerError where
    show (CompilerError loc msg ctxs) = 
        (if knownSpan loc
            then show loc ++ ":\n" else "Compilation error:\n")
        ++ unlines (map ("  " ++) (msg ++ map ("  "++) (reverse ctxs)))

withErrorContext :: String -> Compiler a -> Compiler a
withErrorContext ctx action = action `catchError` addCtx where
    addCtx (CompilerError loc msg ctxs) = throwError $ CompilerError loc msg (ctx:ctxs)

compileError :: MonadError CompilerError m => SrcSpan -> [String] -> m a
compileError loc lines = throwError $ CompilerError loc lines []

panic :: SrcSpan -> [String] -> Compiler a
panic loc msg = compileError loc ("Internal error:" : msg)

checkNameClashes :: SrcSpan -> String -> [LName] -> Compiler ()
checkNameClashes loc msg xs = case findClashes xs of
    [] -> return ()
    xs -> compileError loc $ msg : concatMap showClash xs
    where
        findClashes xs = filter (not . null . tail) . group . sort $ xs
        showClash names = [ "  " ++ unLoc name ++ atLoc name | name <- names ]

debugMessage str = liftIO $ hPutStrLn stderr $ " ** " ++ str

-- Modules -------------------------------------------------------------

data Module = Module (Map Name Export)
data CompiledModule = CM ModuleLoc (Map Name CompiledExport)

newtype ModuleLoc = ModuleLoc (Either ModuleName SrcSpan)
    deriving (Eq, Ord)

namedModule :: ModuleName -> ModuleLoc
namedModule = ModuleLoc . Left

sourceModule :: SrcSpan -> ModuleLoc
sourceModule = ModuleLoc . Right

moduleError :: CompiledModule -> [String] -> Compiler ()
moduleError (CM (ModuleLoc (Left name)) _) xs
    = withErrorContext ("in the module named " ++ name) $ 
        compileError noSpan xs
moduleError (CM (ModuleLoc (Right loc)) _) xs = compileError loc xs

instance Show ModuleLoc where
    show (ModuleLoc (Left name)) = name
    show (ModuleLoc (Right loc)) = show loc

data Export
    = FunExport FunctionDecl
    | VarExport 
    | ReExport LName

data CompiledExport
    = FunCE FunID
    | VarCE VarID
    | ReCE LName
    
loadExternalModules :: Compiler ()
loadExternalModules = do
    paths <- liftIO $ F.find
        (F.depth F.==? 0)
        (F.fileType F.==? F.RegularFile F.&&? F.extension F.==? ".ein")
        "."
    
    modules <- forM paths $ \path -> do
        let name = takeBaseName path
        m <- readModule (show name) path
        return (name, Just m)
    
    modify $ \cs -> cs { csGlobalModules = M.fromList modules }

loadExternalModule :: Name -> Compiler CompiledModule
loadExternalModule name = do
    m <- readModule (show name) (name `addExtension` "ein") 
    modify $ \cs -> cs { csGlobalModules = 
        M.insert name (Just m) (csGlobalModules cs) }
    return m

defineModuleVariable :: Name -> CompiledModule -> Compiler ()
defineModuleVariable name m = do
    mvs <- gets csModuleVariables
    modify $ \cs -> cs { csModuleVariables =
        M.insert name m (csModuleVariables cs) }

defineGlobalModule :: Name -> CompiledModule -> Compiler ()
defineGlobalModule name m = do
    writeModule (name `addExtension` "ein") m
    
    modify $ \cs -> cs 
        { csGlobalModules = M.update (const (Just (Just m))) name (csGlobalModules cs)
        , csNewModules = S.insert name (csNewModules cs)
        }

defineEntryPoint :: Name -> FunName -> CompiledModule -> Compiler ()
defineEntryPoint epName funName (CM _ m) =
    case M.lookup funName m of
        Just (FunCE funID) -> do
            eps <- gets csEntryPoints
            when (M.member epName eps) $ 
                compileError noSpan ["Entry point " ++ show epName ++ ": entry point defined twice"] 
            modify $ \cs -> cs { csEntryPoints = M.insert epName funID eps }
        Just (VarCE _) -> compileError noSpan ["Entry point " ++ show epName ++ ": expected a function, found a variable (" ++ funName ++ " -> breyta)"]
        Just (ReCE (L loc n))  -> compileError loc ["Entry point " ++ show epName ++ ": expected a function, found an unresolved re-export (" ++ funName ++ " -> " ++ n ++ ")"]
        Nothing -> compileError noSpan ["Entry point " ++ show epName ++ ": expected a function, found nothing"]

getEntryPoints :: Compiler [(Name, FunID)]
getEntryPoints = M.toList <$> gets csEntryPoints

moduleVariable :: Name -> Compiler CompiledModule
moduleVariable name = do
    mvs <- gets csModuleVariables
    case M.lookup name mvs of
        Just m -> return m
        Nothing -> compileError noSpan ["Cannot find module variable: " ++ name]

globalModule :: Name -> Compiler CompiledModule
globalModule name = do
    gm <- gets csGlobalModules
    case M.lookup name gm of
        Just (Just m) -> return m
        Just Nothing -> loadExternalModule name
        Nothing -> compileError noSpan ["Cannot find global module " ++ show name]

newVarID :: Compiler VarID
newVarID = do
    i <- gets csNextID
    modify $ \cs -> cs { csNextID = succ i }
    return $ VarID i

-- Global Modules ------------------------------------------------------
    
writeModule :: FilePath -> CompiledModule -> Compiler ()
writeModule path (CM loc m) = do
    (funIDs, _) <- findUsedFunsAndVars (CM loc m)
    liftIO $ encodeFile path (funIDs, m)

findUsedFunsAndVars :: CompiledModule -> Compiler (Map FunID CompiledFunction, Set VarID)
findUsedFunsAndVars (CM _ m) = execStateT (mapM_ findFunsIn (M.elems m)) (M.empty, S.empty)
    where
        findFunsIn (FunCE funID) = do
            (found, _) <- get
            when (not $ M.member funID found) $
                notifyFun funID
        findFunsIn _ = return ()

        notifyVar varID = modify $ second (S.insert varID)

        notifyFun funID = do
            cf <- lift $ retrieveFunction funID
            modify $ first (M.insert funID cf)
            
            CompiledFunction _ _ ir <- lift $ retrieveFunction funID
            forM_ (concatMap funRefs ir) $ \r ->
                case r of
                    ResolvedFun funID ->
                        findFunsIn (FunCE funID)
                    _ -> return ()
            forM_ (concatMap varRefs ir) $ \r ->
                case r of
                    ResolvedVar varID -> notifyVar varID
                    _ -> return ()

type FixupState = (,) (Map FunID FunID) (Map VarID VarID)

readModule :: ModuleName -> FilePath -> Compiler CompiledModule
readModule name path = context $ do
    (funs, exports) <- liftIO $ decodeFile path 
        :: Compiler (Map FunID CompiledFunction, Map Name CompiledExport)
    
    funIDs <- T.mapM defineFunction funs
    
    CM (namedModule name) <$> evalStateT (T.mapM fixIDs exports) (funIDs, M.empty)
    where
        context = withErrorContext $ "when reading global module " ++ name
         
        fixIDs :: CompiledExport -> StateT FixupState Compiler CompiledExport
        fixIDs (VarCE varID) = do
            (_, mapping) <- get
            case M.lookup varID mapping of
                Just varID' -> return (VarCE varID')
                Nothing -> do
                    varID' <- lift newVarID
                    modify $ second (M.insert varID varID')
                    return (VarCE varID')
        
        fixIDs (FunCE funID) = do
            (mapping, _) <- get
            case M.lookup funID mapping of
                Nothing -> lift $ panic noSpan ["Invalid FunID found in module " ++ path]
                Just funID' -> return (FunCE funID')
                
        fixIDs r = return r

-- Compiled Functions --------------------------------------------------

newFunID :: Compiler FunID
newFunID = do
    i <- gets csNextID
    modify $ \cs -> cs { csNextID = succ i }
    return $ FunID i

retrieveFunction :: FunID -> Compiler CompiledFunction
retrieveFunction funID = do
    funs <- gets csFunctions
    case M.lookup funID funs of
        Just fun -> return fun
        Nothing -> panic noSpan ["Internal compiler error: FunID not found"]

defineFunction :: CompiledFunction -> Compiler FunID
defineFunction fun = do
    funID <- newFunID
    modify $ \cs -> cs { csFunctions = M.insert funID fun (csFunctions cs) }
    return funID  

-- Parsing -------------------------------------------------------------

parseProgram :: FilePath -> Compiler Program
parseProgram path = do
    res <- Parser.parseProgram srcName <$> liftIO (readFile path)
    case res of
        Left e -> compileError (toSpan (errorPos e)) (syntaxError : showParsecError e)
        Right r -> return r
    where
        syntaxError = "Syntax error:"
        srcName = takeFileName path
        toSpan pos = join srcSpan (srcLoc srcName (sourceLine pos) (sourceColumn pos))

        showParsecError e = map ("  " ++) . filter (not . null) . lines $ showErrorMessages
            "or" "unknown parse error" "expecting" "unexpected" "end of file"
            (errorMessages e)


---- This has to be quite far down, as apparently you can't define types
---- after splicing in top-level declarations with Template Haskell. 

deriveBinary ''CompiledFunction
deriveBinary ''CompiledExport
deriveBinary ''ModuleLoc
