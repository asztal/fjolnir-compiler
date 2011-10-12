module Language
    ( alphabet
    , operatorAlphabet
    , reservedNames
    , reservedOperators
    , precedences
    ) where

import Data.List ((\\))

alphabet = ['a'..'z'] ++ "þæöðáéýúíó" ++ ['A'..'Z'] ++ "ÞÆÖÐÁÝÚÍÓ_"
operatorAlphabet = "+-*/%!&=?<>|:^@"

reservedOperators = words "-> := ( ) [ ] { } , ; .."
reservedNames = words "annars annarsef breyta eða eflok ef ekki \
    \ fyrir innflutt kostur lykkja lykkjulok meðan og skila staðvær \
    \ stef stofn stofnlok úr út val vallok þá"

-- The precedence level of an operator is determined by the first character
-- of the operator. This list orders those characters by precedence. Any
-- operator starting with a character not found in this list has the lowest
-- precedence level of 1.
-- This list goes from lowest precedence (1) to highest. 
precedences = unassigned : table where
    unassigned = (alphabet ++ ['0'..'9'] ++ operatorAlphabet) \\ concat table
    -- Precedence table given in the Manual, from 2 to 7. 
    -- Note that "!?^@" are valid operator characters, but don't have
    -- an associated precedence level, and thus receive 1.
    table = [ ":", "&", "|", "<>=", "+-", "*/%" ]   
