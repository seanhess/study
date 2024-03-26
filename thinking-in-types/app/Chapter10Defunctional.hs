{-# LANGUAGE FunctionalDependencies #-}

module Chapter10Defunctional where

import Data.Maybe

data Fst a b = Fst (a, b)

class Eval l t | l -> t where
    eval :: l -> t

instance Eval (Fst a b) a where
    eval (Fst (a, _)) = a

data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
    eval (ListToMaybe as) = listToMaybe as
