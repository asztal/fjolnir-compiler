{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Exp
    ( Exp(..)
    , modifyExp
    , modifyExpM
    , flattenExp
    , references
    ) where

import Control.Monad.Identity
import Data.Array.Unboxed
import Data.Either
import Data.Word (Word8, Word16)
import Data.Typeable

import Types

-- A runtime representation of the control flow of a function. This is a
-- simpler representation than the AST, and uses Var and Fun instead of names.
-- Before name resolution, NamedVar and NamedFun are used.
-- During execution, there should be no NamedVar or NamedFun in the tree.
data Exp var fun
    = NilE
    | WordE Word16
    | RealE Double
    | StrE (UArray Int Word8)
    
    | FunE fun Arity
    | ReadE var
    | WriteE var (Exp var fun)
    | AppE (Either var fun) [Either (Exp var fun) var] [Exp var fun]
    | ManyE [Exp var fun]

    -- These are still necessary terms, due to their short-circuiting behaviour.
    | AndE (Exp var fun) (Exp var fun)
    | OrE (Exp var fun) (Exp var fun)
    | NotE (Exp var fun)

    -- For and while loops are converted into simple loops with a conditional break inserted.
    | LoopE (Exp var fun)
    | ReturnE (Exp var fun)
    | BreakE

    -- Case expressions would convert into if expressions, but then you have to encode range testing
    -- in it somehow.
    | CaseE (Exp var fun) [([(Word16, Word16)],Exp var fun)] (Exp var fun)
    | IfE (Exp var fun) (Exp var fun) (Exp var fun)
    deriving Typeable

modifyExpM :: Monad m => (Exp v f -> m (Exp v f)) -> Exp v f -> m (Exp v f)
modifyExpM f e = f =<< case e of
    WriteE v x -> WriteE <$> pure v <*> recur x
    AppE vf refs args -> AppE vf <$> mapM recurRefArg refs <*> recurs args
    ManyE xs -> ManyE <$> recurs xs
    AndE x y -> AndE <$> recur x <*> recur y
    OrE x y -> OrE <$> recur x <*> recur y
    NotE x -> NotE <$> recur x
    LoopE x -> LoopE <$> recur x
    ReturnE x -> ReturnE <$> recur x
    CaseE c cases d -> CaseE <$> recur c <*> mapM recurCase cases <*> recur d
    IfE c x y -> IfE <$> recur c <*> recur x <*> recur y
    x -> return x
    where
        recur x = modifyExpM f x
        recurs xs = mapM recur xs
        recurRefArg (Left x) = Left <$> recur x
        recurRefArg y = return y
        recurCase (ranges, x) = (,) ranges <$> recur x

        infixl 4 <$>
        (<$>) = liftM
        infixl 4 <*>
        (<*>) = ap
        pure = return
        

modifyExp :: (Exp v f -> Exp v f) -> Exp v f -> Exp v f
modifyExp f = runIdentity . modifyExpM (Identity . f)

references :: Exp v f -> ([v], [f])
references = partitionEithers . concatMap g . flattenExp where
    -- We only need to check for constructors that include a Var or Fun directly.
    -- Flattening the expression does the recursion for us.
    g (FunE f _) = [Right f]
    g (ReadE v) = [Left v]
    g (WriteE v _) = [Left v]
    g (AppE vf _ _) = [vf]
    g _ = []

flattenExp :: Exp v f -> [Exp v f]
flattenExp e = (e :) . concatMap flattenExp $ case e of
    WriteE _ x -> [x]
    AppE _ refs args -> lefts refs ++ args
    ManyE xs -> xs
    AndE x y -> [x,y]
    OrE x y -> [x,y]
    NotE x -> [x]
    LoopE x -> [x]
    ReturnE x -> [x]
    CaseE x cases d -> x : d : map snd cases
    IfE x y z -> [x,y,z]
    _ -> []

