{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CGen
    ( writeEntryPoint
    ) where

import Control.Applicative
import Control.Monad (forM, forM_, when)
import Control.Monad.Error (MonadError)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Control.Monad.Writer (WriterT, execWriterT, MonadWriter(tell))
import Data.List (intercalate, intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq 
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Foldable as F
import System.FilePath (addExtension)
import System.IO (hPutStr, withFile, IOMode(..))

import Compiler
import Module
import Exp
import IR
import Var
import Types

newtype GenC a = GenC { unGenC :: WriterT (Seq String) Compiler a }
    deriving (Functor, Monad, Applicative, MonadIO, MonadWriter (Seq String), MonadError CompilerError)
    
write :: String -> GenC ()
write = tell . Seq.singleton

newLine :: GenC ()
newLine = write "\n"

genC :: GenC () -> Compiler (Seq String) 
genC (GenC x) = execWriterT x

writeCFile :: FilePath -> GenC () -> Compiler ()
writeCFile path x = do
    chunks <- genC x
    liftIO $ withFile path WriteMode $ \h -> do
        F.mapM_ (hPutStr h) chunks

writeEntryPoint :: Name -> FunID -> Compiler ()
writeEntryPoint name funID@(FunID funID') = context $ do    
    CompiledFunction arity _ _ <- retrieveFunction funID
    when (arity /= (0,0)) $ do
        compileError noSpan ["Entry point \"" ++ name ++ "\" has an arity of " ++ show arity]
    
    (usedFuns, (usedVars, nativeFuns)) <- findUsedFunsAndVars (CM (namedModule "$START") (M.singleton "$start" (FunCE funID)))
    
    writeCFile filePath $ do
        write "#include \"mjollnir.h\"\n\n"
        
        mapM_ writeNativeProto (S.toList nativeFuns)
        newLine
        
        mapM_ (uncurry (writeProto ";")) (M.toList usedFuns)
        newLine
        
        write $ "int main() { F" ++ show funID' ++ "(); return 0; }\n"
        newLine
        
        writeVarDefs (S.toList usedVars)
        mapM_ ((>> newLine) . uncurry writeC) (M.toList usedFuns) 
    
    where
        context = withErrorContext $ "when writing the C file " ++ filePath
        filePath = name `addExtension` "c"
    
writeVarDefs xs = forM_ xs $ \(VarID varID) -> write $    
    "Value V" ++ show varID ++ " = nil;\n"

writeProto suffix funID cf = write $ generateProto funID cf ++ suffix ++ "\n"
    where
        generateProto (FunID funID) (CompiledFunction arity _ _) =
            "Value F" ++ show funID ++ " (" ++ argList arity ++ ")"

writeNativeProto (NativeFunction cName _ arity) = write $
    "Value " ++ cName ++ " (" ++ argList arity ++ ");\n"

nameList prefix suffix count = intercalate ", " [ prefix ++ show i ++ suffix | i <- [0..count-1]]
argList arity = intercalate ", " . filter (not . null) $
    [ nameList "Value *R" "" (fst arity)
    , nameList "Value A" "" (snd arity)]

writeC :: FunID -> CompiledFunction -> GenC ()
writeC (FunID funID) cf@(CompiledFunction arity _locals instructions) = do
    writeProto " {" (FunID funID) cf
    writeStackVars
    writeLocalVars
    mapM_ (uncurry writeInstruction) $ zip [0..] instructions 
    
    write $ "  I" ++ show (length instructions) ++ ": return S0;\n"
    write "}\n"
    where
        writeStackVars = write $ "  Value " ++ nameList "S" "" (stackVarCount instructions) ++ ";\n"
        writeLocalVars = case localVarCount instructions of
            0 -> write ""
            count -> write $ "  Value " ++ nameList "L" " = nil" count ++ ";\n"

        stackVarCount xs = foldr1 max (0 : concatMap getStackRefs xs) + 1
        localVarCount xs = foldr max (-1) (concatMap getLocalRefs xs) + 1
        
        getStackRefs x = concatMap f (varRefs x) where
            f (StackVar i) = [i]
            f _ = []
            
        getLocalRefs x = concatMap f (varRefs x) where
            f (LocalVar i) = [i]
            f _ = []
            
writeInstruction :: Int -> Instruction Int -> GenC ()
writeInstruction i instruction = write ("  I" ++ show i ++ ": ") >> f instruction >> write ";\n" where
    f (WordI v w) = writeVar v >> write " = makeWord(" >> write (show w ++ ")")
    f (StringI v s) = writeVar v >> write " = makeString(" >> write (show s ++ ")")
    f (NilI v) = writeVar v >> write " = nil"
    f (RealI v x) = writeVar v >> write " = makeReal(" >> write (show x) >> write "d)"
    f (AssignI u v) = writeVar u >> write " = " >> writeVar v
    f (JumpI l) = write $ "goto I" ++ show l
    f (ConditionalI v l) = write "if (" >> writeVar v >> write ") goto I" >> write (show l)
    f (CallI v f vs vs') = do
        writeVar v
        write " = "
        writeFun f
        write "(" 
        sequence_ $ intersperse (write ",") (map writeRefArg vs ++ map writeVar vs')
        write ")"
    f (CallVI v f vs vs') = do
        writeVar v
        write " = (("
        write (typeSig (length vs, length vs')) 
        write ")(loadStef(" 
        writeVar f
        write ","
        write $ show (length vs) ++ "," ++ show (length vs') ++ ")))("
        sequence_ $ intersperse (write ",") (map writeRefArg vs ++ map writeVar vs')
        write ")"
    f (LoadFunI v f (m,n)) = do
        writeVar v 
        write " = makeStef(&" 
        writeFun f 
        write "," 
        write $ show m ++ "," ++ show n ++ ")" 
    f _ = write "/* TODO */"
    
    typeSig (m,n) = "Value(*)(" ++ intercalate "," (ms ++ ns) ++ ")" where
        ms = replicate m "Value*"
        ns = replicate n "Value"
    
    writeRefArg :: Var -> GenC ()
    writeRefArg v = write "&" >> writeVar v
    
    writeVar :: Var -> GenC ()
    writeVar (ImportedVar (L loc name)) = compileError loc ["Unresolved variable " ++ show name ++ " encountered"]
    writeVar (LocalVar j) = write $ "L" ++ show j
    writeVar (ArgVar j) = write $ "A" ++ show j
    writeVar (RefArgVar j) = write $ "*R" ++ show j
    writeVar (StackVar j) = write $ "S" ++ show j
    writeVar (ResolvedVar (VarID varID)) = write $ "V" ++ show varID
    
    writeFun :: Fun -> GenC ()
    writeFun (ImportedFun (L loc name) _) = compileError loc ["Unresolved function " ++ show name ++ " encountered"]
    writeFun (ResolvedFun (FunID funID)) = write $ "F" ++ show funID
    writeFun (ResolvedNativeFun (NativeFunction cName _ _)) = write cName
