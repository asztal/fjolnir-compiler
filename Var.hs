{-# LANGUAGE TemplateHaskell #-}

module Var
    ( VarID(..)
    , FunID(..)
    , Var(..)
    , Fun(..)
    ) where

import DeriveBinary
import Types

newtype VarID = VarID Int
    deriving (Eq, Ord, Show)
    
newtype FunID = FunID Int
    deriving (Eq, Ord, Show)
    
deriveBinary ''VarID
deriveBinary ''FunID

data Var
    = LocalVar Int
    | ArgVar Int
    | RefArgVar Int
    | StackVar Int
    
    | ImportedVar LVarName
    | ResolvedVar VarID
    deriving Show
    
deriveBinary ''Var

data Fun
    = ImportedFun LFunName Arity
    | ResolvedFun FunID
    deriving Show
    
deriveBinary ''Fun
