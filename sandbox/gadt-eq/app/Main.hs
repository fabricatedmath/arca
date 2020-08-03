{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Type.Equality
import Data.Typeable

data Some t = forall a. Some (t a)

--deriving instance Eq (Some AST)

instance TestEquality f => Eq (Some f) where
    Some fa == Some fb = case testEquality fa fb of
        Just Refl -> True
        Nothing   -> False

data AST o where
    Literal :: Typeable o => o -> AST o


instance TestEquality AST where
    testEquality (Literal o1) (Literal o2) = eqT
--deriving instance Eq (AST o)
{-
sameGadtType :: AST o1 -> AST o2 -> Maybe (AST o1 :~: AST o2)
sameGadtType a b = 
    case a of
        Literal{} -> case b of
            Literal{} -> Just Refl
            _ -> Nothing
            -}

main :: IO ()
main = pure ()
