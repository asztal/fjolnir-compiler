{-# LANGUAGE TemplateHaskell #-}

module DeriveBinary
    ( deriveBinary
    ) where

import Control.Monad (replicateM)
import Data.Binary
import Data.Char

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

deriveBinary :: Name -> Q [Dec]
deriveBinary ty = do
    (TyConI dec) <- reify ty
    let (tyName, tyVars, tyCons) = typeInfo dec
    
    inst <- instanceD 
        (cxt $ [ classP ''Binary [varT v] | v <- tyVars ])
        (appT (conT ''Binary)
            (return (foldl1 AppT $ ConT ty : map VarT tyVars)))
        [genGet tyCons, genPut tyCons]
        
    return [inst]
    
    where
    
    getN, putN :: Name
    getN = 'get
    putN = 'put
    
    genGet, genPut :: [Con] -> DecQ
    genPut cons = funD putN clauses where
        clauses = map (uncurry makeClause) $ zip [0..] cons
        makeClause conIndex (NormalC conName types) = do
            names <- replicateM (length types) (newName "a")
            clause 
                [conP conName (map varP names)] 
                (normalB $ doE $ putConIndex : map putConArg names)
                []
            where
                putConIndex = noBindS $ appE (varE 'putWord8) (litE (IntegerL conIndex)) -- '
                putConArg name = noBindS $ appE (varE putN) (varE name)
    
    genGet cons = valD (varP getN) (normalB $ body) [] where
        body = do
            selector <- newName "i"
            -- get :: Word8
            let selE = varE 'getWord8 -- '
            doE 
                [ bindS (varP selector) selE
                , noBindS $ caseE
                    (varE selector) 
                    ((map (uncurry matcher) (zip [0..] cons)) ++ [failer])]
            where
                matcher conIndex (NormalC conName types) = do
                    names <- replicateM (length types) (newName "a")
                    let body = doE $ 
                            [ bindS (varP n) (varE getN) | n <-  names] 
                            ++ [ noBindS (appE (varE 'return) -- '
                                (appsE $ conE conName : map varE names))] 
                    match
                        (litP (IntegerL conIndex)) 
                        (normalB body)
                        []
                        
                failer = match wildP (normalB [|fail "Binary file in incorrect format"|]) []
    
    typeInfo :: Dec -> (Name, [Name], [Con])
    typeInfo (DataD _ name typeVars cons _) = (name, map tvName typeVars, cons)
    typeInfo (NewtypeD _ name typeVars con _) = (name, map tvName typeVars, [con])
    typeInfo otherDec = error $ "deriveBinary: not a type declaration: " ++ show otherDec
    
    tvName :: TyVarBndr -> Name 
    tvName (PlainTV name) = name
    tvName (KindedTV name _) = name
