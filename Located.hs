{-# LANGUAGE BangPatterns, EmptyDataDecls, DeriveDataTypeable,
             TemplateHaskell #-}

module Located
    (
      SrcLoc, srcLoc, noSrcLoc, unknownLoc,
      SrcSpan, span, srcSpan, spanStart, spanEnd,
               noSpan, genSpan, combineSpan, knownSpan, unknownSpan,
      Row, Col,
      Located(L), noLoc, getLoc, genLoc, unLoc, withLoc,
                  sameLoc, cmpLoc, mapLoc, combineLoc, atLoc, inlineLoc
    ) where

import Control.Applicative
import Data.Data (Data, Typeable)
import Data.Maybe (fromMaybe)
import Prelude hiding (span)

import DeriveBinary

type Row = Int
type Col = Int

data SrcLoc
    = SrcLoc String Row Col
    | UnknownLoc String
    deriving (Typeable, Data)
    
deriveBinary ''SrcLoc

srcLoc = SrcLoc
noSrcLoc = UnknownLoc "<no location info>"
unknownLoc = UnknownLoc

instance Eq SrcLoc where
    UnknownLoc _ == UnknownLoc _ = True
    SrcLoc src row col == SrcLoc src' row' col'
        = src == src' && row == row' && col == col'
    _ == _ = False

instance Ord SrcLoc where
    compare (UnknownLoc _) (UnknownLoc _) = EQ
    compare (UnknownLoc _) _ = LT
    compare _ (UnknownLoc _) = GT
    compare (SrcLoc _ row col) (SrcLoc _ row' col')
        = compare [row,col] [row',col']

instance Show SrcLoc where
    show (SrcLoc src row col) = show (Point src row col)
    show (UnknownLoc x) = x

data SrcSpan
    = OneLine String !Row !Col !Col
    | MultiLine String !Row !Col !Row !Col
    | Point String !Row !Col
    | UnknownSpan String
    deriving (Eq, Typeable, Data)

deriveBinary ''SrcSpan

span from@(SrcLoc src row col) to@(SrcLoc _ row' col')
    | row > row' = span to from
    | row == row' && col > col' = span to from
    | row' > row = MultiLine src row col row' col'
    | col' > col = OneLine src row col col'
    | otherwise = Point src row col
span (UnknownLoc x) _ = UnknownSpan x
span _ (UnknownLoc x) = UnknownSpan x

srcSpan = span

spanSourceName (OneLine src _ _ _) = src
spanSourceName (MultiLine src _ _ _ _) = src
spanSourceName (Point src _ _) = src
spanSourceName (UnknownSpan src) = src

spanStart (OneLine _ row col _) = Just (row, col)
spanStart (MultiLine _ row col _ _) = Just (row, col)
spanStart (Point _ row col) = Just (row, col)
spanStart _ = Nothing

spanEnd (OneLine _ row _ col') = Just (row, col')
spanEnd (MultiLine _ _ _ row' col') = Just (row', col')
spanEnd (Point _ row col) = Just (row, col+1)
spanEnd _ = Nothing

combineSpan first second =
    fromMaybe first $ combined <|> try first <|> try second
    where
        src = spanSourceName first
        try (UnknownSpan _) = Nothing
        try x = Just x
        combined = do
            (row, col) <- spanStart first
            (row', col') <- spanEnd second
            return $ span (SrcLoc src row col) (SrcLoc src row' col')

instance Ord SrcSpan where
    compare x y = compare [spanStart x, spanEnd x] [spanStart y, spanEnd y]

noSpan = UnknownSpan "<no location info>"
genSpan = UnknownSpan "<compiler-generated>"
unknownSpan = UnknownSpan
knownSpan (UnknownSpan _) = False
knownSpan _ = True

instance Show SrcSpan where
    show (OneLine src row col _) =
        src ++ ":" ++ show row ++ ":" ++ show col -- ++ "-" ++ show col'
    show (MultiLine src row col _ _) =
        src ++ ":" ++ show row ++ ":" ++ show col -- ++ "-" ++ show row' ++ ":" ++ show col'
    show (Point src row col) =
        src ++ ":" ++ show row ++ ":" ++ show col
    show (UnknownSpan x) = x

data Located a = L SrcSpan a
    deriving (Typeable, Data)
    
deriveBinary ''Located

instance Eq a => Eq (Located a) where
    L _ x == L _ y = x == y
    L _ x /= L _ y = x /= y

instance Ord a => Ord (Located a) where
    compare (L _ x) (L _ y) = compare x y

instance Functor Located where
    fmap f (L l x) = L l (f x)

instance Show x => Show (Located x) where
    showsPrec p (L _ x) = showsPrec p x
    show (L _ x) = show x
    showList xs = showList [x | L _ x <- xs]

unLoc (L _ x) = x
getLoc (L l _) = l
genLoc x = L (UnknownSpan "<compiler-generated>") x
noLoc x = L noSpan x
withLoc (L l _) x = L l x
sameLoc (L x _) (L y _) = x == y
cmpLoc (L x _) (L y _) = compare x y
mapLoc f (L l x) = L l (f x)
combineLoc (L a _) (L b _) z = L (combineSpan a b) z

atLoc (L sp _) = inlineLoc sp

inlineLoc sp
    | knownSpan sp = " (at " ++ show sp ++ ")"
    | otherwise = ""
