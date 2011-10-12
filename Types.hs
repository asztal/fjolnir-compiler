module Types
    ( Name, LName
    , VarName, LVarName
    , FunName, LFunName
    , Arity
    ) where

import Located

type Name = String
type LName = Located Name
type VarName = Name
type LVarName = Located VarName
type FunName = Name
type LFunName = Located FunName
type Arity = (Integer, Integer)
