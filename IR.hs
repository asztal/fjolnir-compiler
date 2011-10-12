{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor,
             RankNTypes, TemplateHaskell #-}

module IR (
    Instruction(..),
    compileToIR,
    varRefs, funRefs, modifyIRVarRefs
    ) where

import Control.Applicative
import Control.Monad.ST
import Control.Monad.State
import qualified Data.Array.Unboxed
import Data.Char (chr)
import qualified Data.Foldable as F
import Data.Maybe (maybeToList)
import qualified Data.Sequence as S
import Data.STRef
import qualified Data.Traversable as T
import Data.Word (Word16)

import DeriveBinary
import Exp
import Types
import Var

instance Applicative (ST s) where
    pure = return
    (<*>) = ap

-- IR, like Exp, represents the control flow of a function,
-- but transformed into an imperative style intermediate
-- representation that is easier to compile to byte code.
-- 
-- IR does not need to be generalised to work with both IVar 
-- and CVar because only the compiler uses this representation.
data Instruction label
    = WordI Var Word16
    | StringI Var String
    | NilI Var
    | RealI Var Double
    
    | AssignI Var Var
    | JumpI label
    | ConditionalI Var label
    | CallI Var Fun [Var] [Var]
    | CallVI Var Var [Var] [Var]
    
    -- Corresponds to x := stef f (0;2)
    | LoadFunI Var Fun Arity
    
    -- Used for computing addresses for passing a variable with
    -- reference semantics, e.g.
    --   staðvær x
    --   f(x;),
    --   skrifa(;x),
    -- would become something like
    --   LoadAddressI (StackVar 1) (StackVar 0)
    --   CallI Nothing f [StackVar 1] []
    --   CallI Nothing skrifa [] [StackVar 0]
    | LoadAddressI Var Var
    
    -- For testing ranges in case expressions
    | InRangeI Var Var Word16 Word16
    deriving (Show)

deriveBinary ''Instruction

compileToIR :: Exp Var Fun -> [Instruction Int]
compileToIR x = runGenIR (flip generateIR x =<< retVal)

modifyIRVarRefs :: (Monad m, Applicative m) 
    => (Var -> m Var) 
    -> (Fun -> m Fun) 
    -> Instruction a 
    -> m (Instruction a)
modifyIRVarRefs vf ff instr = case instr of
    WordI v w -> WordI <$> vf v <*> pure w
    StringI v s -> StringI <$> vf v <*> pure s
    NilI v -> NilI <$> vf v
    RealI v r -> RealI <$> vf v <*> pure r
    AssignI u v -> AssignI <$> vf u <*> vf v
    JumpI l -> return (JumpI l)
    ConditionalI v l -> ConditionalI <$> vf v <*> pure l
    CallI v f vs vs' -> CallI <$> vf v <*> ff f <*> mapM vf vs <*> mapM vf vs'
    CallVI v f vs vs' -> CallVI <$> vf v <*> vf f <*> mapM vf vs <*> mapM vf vs'
    LoadFunI v f a -> LoadFunI <$> vf v <*> ff f <*> pure a
    LoadAddressI u v -> LoadAddressI <$> vf u <*> vf v
    InRangeI u v x y -> InRangeI <$> vf u <*> vf v <*> pure x <*> pure y

varRefs :: Instruction a -> [Var]
varRefs (NilI v) = [v]
varRefs (WordI v _) = [v]
varRefs (RealI v _) = [v]
varRefs (StringI v _) = [v]
varRefs (AssignI u v) = [u,v]
varRefs (ConditionalI v _) = [v]
varRefs (CallI rv _ vs vs') = rv : vs ++ vs'
varRefs (CallVI rv v vs vs') = v:(rv : vs ++ vs')
varRefs (LoadFunI v _ _)  = [v]
varRefs (LoadAddressI u v) = [u,v]
varRefs (InRangeI u v _ _) = [u,v]
varRefs _ = []

funRefs :: Instruction a -> [Fun]
funRefs (CallI _ f _ _) = [f]
funRefs _ = []

newtype Label s = Label (STRef s Int) 

newtype GenIR s a = GenIR { unGenIR :: StateT (GenState s) (ST s) a } 
    deriving (Monad, Applicative, Functor, MonadState (GenState s))
    
data GenState s = GenState 
    { gsInstructions :: S.Seq (Instruction (Label s)) 
    , gsBreakLabel :: Maybe (Label s)
    , gsReturnLabel :: Label s
    , gsStackIndex :: Int
    , gsRetVal :: Var
    }
    
runGenIR :: (forall s. GenIR s ()) -> [Instruction Int]
runGenIR x = runST (do
    end <- Label <$> newSTRef (-1)
    gs <- execStateT
        (unGenIR (do
            rv <- stackVar
            modify $ \gs -> gs { gsRetVal = rv }
            x
            defineLabel end))
        (GenState S.empty Nothing end 0 undefined)
    mapM resolveLabel (F.toList (gsInstructions gs)))

resolveLabel :: Instruction (Label s) -> ST s (Instruction Int)
resolveLabel (JumpI (Label r)) = JumpI <$> readSTRef r
resolveLabel (ConditionalI v (Label r)) = ConditionalI v <$> readSTRef r
resolveLabel (WordI v x) = return $ WordI v x
resolveLabel (RealI v x) = return $ RealI v x
resolveLabel (StringI v x) = return $ StringI v x
resolveLabel (NilI v) = return $ NilI v
resolveLabel (AssignI u v) = return $ AssignI u v
resolveLabel (CallI r f vs vs') = return $ CallI r f vs vs'
resolveLabel (CallVI r v vs vs') = return $ CallVI r v vs vs'
resolveLabel (LoadAddressI u v) = return $ LoadAddressI u v
resolveLabel (InRangeI u v low high) = return $ InRangeI u v low high
resolveLabel (LoadFunI x f a) = return $ LoadFunI x f a

-- This linearises the expression, but does not put it into SSA form.
generateIR :: Var -> Exp Var Fun -> GenIR s ()
generateIR output expr = case expr of
    NilE -> do
        write $ NilI output
    WordE w -> write $ WordI output w
    StrE s -> write $ StringI output 
        (map (chr . fromIntegral) (Data.Array.Unboxed.elems s))
    RealE r -> write $ RealI output r
    
    FunE f arity -> do
        x <- stackVar
        -- TODO check arity?
        write $ LoadFunI x f arity
    
    ReadE v -> write $ AssignI output v
    WriteE v e -> do
        generateIR v e
        write $ AssignI output v
    
    AppE (Left v) rs xs -> do
        rs' <- mapM evalRefArg rs
        xs' <- mapM evalValArg xs
        write $ CallVI output v rs' xs'
    
    -- TODO: Check arity is correct.
    AppE (Right f) rs xs -> do
        rs' <- mapM evalRefArg rs
        xs' <- mapM evalValArg xs
        write $ CallI output f rs' xs'
        
    ManyE xs -> do
        mapM_ (generateIR output) xs
    
    AndE x y -> do
        x' <- stackVar
        generateIR x' x
        end <- label
        success <- label
        tryY <- label
        
        write $ ConditionalI x' tryY
        write $ NilI output
        jmp end
        
        defineLabel tryY
        y' <- stackVar
        generateIR y' y
        write $ ConditionalI y' success
        write $ NilI output
        jmp end
        
        defineLabel success
        write $ AssignI output y'
        defineLabel end

    OrE x y -> do
        x' <- stackVar
        done <- label
        end <- label
        generateIR x' x
        write $ ConditionalI x' done
        generateIR output y
        jmp end
        defineLabel done
        write $ AssignI output x'
        defineLabel end
        
    NotE x -> do
        x' <- stackVar
        done <- label
        end <- label
        generateIR x' x
        write $ ConditionalI x' done
        write $ WordI output 1
        jmp end
        defineLabel done
        write $ NilI output
        defineLabel end
        
    LoopE x -> do
        startOfLoop <- label
        endOfLoop <- label
        defineLabel startOfLoop
        withBreakLabel endOfLoop $ do
            generateIR output x
            jmp startOfLoop
        defineLabel endOfLoop 
        
    BreakE -> do
        breakLabel <- gets gsBreakLabel
        case breakLabel of 
            -- TODO: add error handling to GenIR?
            Nothing -> error "\"út\" outside of loop"
            Just l -> jmp l
            
    ReturnE x -> do
        r <- retVal
        end <- gets gsReturnLabel
        generateIR r x
        jmp end
            
    IfE cond x y -> do
        cond' <- stackVar
        condTrue <- label
        end <- label
        generateIR cond' cond
        write $ ConditionalI cond' condTrue
        generateIR output y
        jmp end 
        defineLabel condTrue
        generateIR output x
        defineLabel end  
        
    CaseE scrutinee branches defaultBranch -> do
        scrutinee' <- stackVar
        generateIR scrutinee' scrutinee
        
        defaultBranchLabel <- label
        end <- label
        labels <- forM branches (\branch -> (,) <$> label <*> pure branch)
        
        -- Emit the code that selects the correct branch.
        ir <- stackVar
        forM_ labels $ \(branchLabel, (ranges, _)) -> do
            forM_ ranges $ \(low, high) -> do
                write $ InRangeI ir scrutinee' low high
                write $ ConditionalI ir branchLabel
        
        jmp defaultBranchLabel
        
        -- Now emit the IR for each branch.
        forM_ labels $ \(branchLabel, (_, body)) -> do
            defineLabel branchLabel
            generateIR output body
            write $ JumpI end
            
        defineLabel defaultBranchLabel
        generateIR output defaultBranch
        
        defineLabel end
    
    where
        evalRefArg :: Either (Exp Var Fun) Var -> GenIR s Var
        evalRefArg (Right var) = return var
        evalRefArg (Left arg) = do
            x <- stackVar
            generateIR x arg
            evalRefArg (Right x)
        
        evalValArg :: Exp Var Fun -> GenIR s Var 
        evalValArg arg = do
            x <- stackVar
            generateIR x arg
            return x
                        
        withBreakLabel :: Label s -> GenIR s () -> GenIR s ()
        withBreakLabel breakLabel action = do
            oldBreakLabel <- gets gsBreakLabel
            modify (\gs -> gs { gsBreakLabel = Just breakLabel })
            action
            modify (\gs -> gs { gsBreakLabel = oldBreakLabel })
             

label :: GenIR s (Label s)
label = GenIR . lift $ (Label <$> newSTRef (-1))

jmp :: Label s -> GenIR s ()
jmp l = write $ JumpI l

stackVar :: GenIR s Var
stackVar = do
    i <- gets gsStackIndex
    modify $ \gs -> gs { gsStackIndex = succ i }
    return $ StackVar i

retVal :: GenIR s Var
retVal = gets gsRetVal

write :: Instruction (Label s) -> GenIR s ()
write instr = GenIR . modify $ \ gs -> 
    gs { gsInstructions = gsInstructions gs S.|> instr }

defineLabel :: Label s -> GenIR s ()
defineLabel (Label r) = do
    instrs <- GenIR $ gets gsInstructions
    GenIR . lift $ writeSTRef r (S.length instrs)
