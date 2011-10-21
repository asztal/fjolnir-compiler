{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Var
    ( VarID(..)
    , FunID(..)
    , Var(..)
    , Fun(..)
    , NativeFunction(..)
    ) where

import Data.Binary
import DeriveBinary
import Types

newtype VarID = VarID Int
    deriving (Eq, Ord, Show, Binary)
    
newtype FunID = FunID Int
    deriving (Eq, Ord, Show, Binary)
    
data Var
    = LocalVar Int
    | ArgVar Int
    | RefArgVar Int
    | StackVar Int
    
    | ImportedVar LVarName
    | ResolvedVar VarID
    deriving Show
    
deriveBinary ''Var

data NativeFunction = NativeFunction FunName Name Arity
    deriving (Show, Eq, Ord)

deriveBinary ''NativeFunction

data Fun
    = ImportedFun LFunName Arity
    | ResolvedNativeFun NativeFunction
    | ResolvedFun FunID
    deriving Show
    
deriveBinary ''Fun
