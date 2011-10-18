module Main where

import Control.Applicative
import Control.Monad (forM, forM_)
import Control.Monad.Trans (liftIO)
import System.IO (hPutStrLn, stderr)
import System.Environment.UTF8 (getArgs)

import AST
import Compiler
import CGen
import Exp
import IR
import Var
import Module

main :: IO ()
main = compile =<< getArgs

compile paths = do
    result <- runCompiler $ do
        loadExternalModules -- "MYMODULE.ein"
        
        forM_ paths $ \path -> do
            syn <- parseProgram path
            forM_ syn (performProgramStatement . unLoc)
        
        entryPoints <- getEntryPoints
        
        case entryPoints of
            [] -> return ()
            es -> do
                forM_ es $ \(name, funID) -> do
                    debugMessage $ "Creating C file: " ++ name ++ ".c"
                    writeEntryPoint name funID
        
    case result of
        Left err -> hPutStrLn stderr (show err)
        Right r -> return r
