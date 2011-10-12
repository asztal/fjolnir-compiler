module CGen
    ( generateC
    , writeEntryPoint
    ) where

import Control.Monad (forM, when)
import Control.Monad.Trans (liftIO)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import System.FilePath (addExtension)


import Compiler
import Module
import Exp
import IR
import Var
import Types

sampleExp :: Exp Var Fun
sampleExp = AndE (WordE 2) (OrE (NotE (RealE 7.0)) (IfE (ReadE (LocalVar 5)) (ReadE (RefArgVar 0)) (WordE 6)))

writeEntryPoint :: Name -> FunID -> Compiler ()
writeEntryPoint name funID@(FunID funID') = context $ do    
    CompiledFunction arity _ _ <- retrieveFunction funID
    when (arity /= (0,0)) $ do
        compileError noSpan ["Entry point \"" ++ name ++ "\" has an arity of " ++ show arity]
    
    (usedFuns, usedVars) <- findUsedFunsAndVars (CM (M.singleton "$start" (FunCE funID)))
    let funDefs = map (uncurry generateC) (M.toList usedFuns)
        varDefs = generateVarDefs (S.toList usedVars)
        protos = map ((++ ";") . uncurry generateProto) (M.toList usedFuns)
        main = "int main() { F" ++ show funID' ++ "(); return 0; }\n" 
        header = "#include \"mjollnir.h\"\n\n"
    
    liftIO $ writeFile filePath (header ++ unlines protos ++ "\n" ++ main ++ varDefs ++ "\n" ++ unlines funDefs)
    where
        context = withErrorContext $ "when writing the C file " ++ filePath
        filePath = name `addExtension` "c"
    
generateVarDefs xs = unlines    
    [ "Value V" ++ show varID ++ " = nil;" | VarID varID <- xs ]

generateProto (FunID funID) (CompiledFunction arity _ _) =
    "Value F" ++ show funID ++ " (" ++ argList arity ++ ")"

nameList prefix suffix count = intercalate ", " [ prefix ++ show i ++ suffix | i <- [0..count-1]]
argList arity = intercalate ", " . filter (not . null) $
    [ nameList "Value *R" "" (fst arity)
    , nameList "Value A" "" (snd arity)]

generateC :: FunID -> CompiledFunction -> String
generateC (FunID funID) cf@(CompiledFunction arity _locals instructions) = 
    generateProto (FunID funID) cf ++ " {\n"
    ++ stackVars
    ++ localVars
    ++ concatMap (uncurry genInstr) (zip [0..] instructions)
    ++ "  I" ++ show (length instructions) ++ ": return S0;\n"
    ++ "}\n"
    where
        stackVars = "  Value " ++ nameList "S" "" (stackVarCount instructions) ++ ";\n"
        localVars = case localVarCount instructions of
            0 -> ""
            count -> "  Value " ++ nameList "L" " = nil" count ++ ";\n"
    
        stackVarCount xs = foldr1 max (0 : concatMap getStackRefs xs) + 1
        localVarCount xs = foldr max (-1) (concatMap getLocalRefs xs) + 1
        
        getStackRefs x = concatMap f (varRefs x) where
            f (StackVar i) = [i]
            f _ = []
            
        getLocalRefs x = concatMap f (varRefs x) where
            f (LocalVar i) = [i]
            f _ = []
    
genInstr :: Int -> Instruction Int -> String
genInstr i instruction = "  I" ++ show i ++ ": " ++ f instruction ++ ";\n" where
    f (WordI v w) = showVar v ++ " = makeWord(" ++ show w ++ ")"
    f (StringI v s) = showVar v ++ " = makeString(" ++ show s ++ ")"
    f (NilI v) = showVar v ++ " = nil"
    f (RealI v x) = showVar v ++ " = makeReal(" ++ show x ++ "d)"
    f (AssignI u v) = showVar u ++ " = " ++ showVar v
    f (JumpI l) = "goto I" ++ show l
    f (ConditionalI v l) = "if (" ++ showVar v ++ ") goto I" ++ show l
    f (CallI v f vs vs') = showVar v ++ " = " ++ showFun f ++ "(" ++ intercalate "," (map showRefArg vs ++ map showVar vs') ++ ")"
    f (CallVI v f vs vs') = showVar v ++ " = ((Value(*)())(loadStef(" ++ showVar f ++ "," ++ show (length vs) ++ "," ++ show (length vs') ++ ")))(" ++ intercalate "," (map showRefArg vs ++ map showVar vs') ++ ")"
    f (LoadFunI v f (m,n)) = showVar v ++ " = makeStef(&" ++ showFun f ++ "," ++ show m ++ "," ++ show n ++ ")" 
    f _ = "/* TODO */"
    
    showRefArg :: Var -> String
    showRefArg v = "&" ++ showVar v
    
    showVar :: Var -> String
    showVar (ImportedVar _) = error "ImportedVar present at C generation stage"
    showVar (LocalVar j) = "L" ++ show j
    showVar (ArgVar j) = "A" ++ show j
    showVar (RefArgVar j) = "*R" ++ show j
    showVar (StackVar j) = "S" ++ show j
    showVar (ResolvedVar (VarID varID)) = "V" ++ show varID
    
    showFun :: Fun -> String
    --showFun (ImportedFun (L _ name) _) = name
    showFun (ImportedFun (L _ name) _) = error $ "ImportedFun " ++ name ++ " present at C generation stage"
    showFun (ResolvedFun (FunID funID)) = "F" ++ show funID
