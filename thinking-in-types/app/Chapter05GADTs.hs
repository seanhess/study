{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter05GADTs where

import Data.Kind (Constraint, Type)

test :: IO ()
test = putStrLn "TEST"

-- Heterogenous List. You can specify and collect the type in the first parameter
data HList (ts :: [Type]) where
    HNil :: HList '[]
    (:|) :: t -> HList ts -> HList (t : ts)
infixr 5 :|

hHead :: HList (t : ts) -> t
hHead (t :| _) = t

example :: HList [Maybe [Char], Bool]
example = Just "hello" :| False :| HNil

-- Base case: only for empty HList
instance Eq (HList '[]) where
    HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t : ts)) where
    (a :| as) == (b :| bs) = a == b && as == bs

-- instance Show (HList '[]) where
--     show HNil = "HNil"
--
-- instance (Show t, Show (HList ts)) => Show (HList (t : ts)) where
--     show (t :| ts) = show t ++ " :| " ++ show ts

-- instance Ord (HList '[]) where
--     HNil <= HNil = True
--
-- instance (Ord t, Ord (HList ts)) => Ord (HList (t : ts)) where
--     (a :| as) <= (b :| bs) = (a <= b) && (as <= bs)

-- Instead: Type Family All
type family
    All
        (c :: Type -> Constraint)
        (ts :: [Type]) ::
        Constraint
    where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

instance (All Show ts) => Show (HList ts) where
    show HNil = "HNil"
    show (t :| ts) = show t ++ " :| " ++ show ts

-- instance (All Eq ts, All Ord ts) => Ord (HList ts) where
--     compare HNil HNil = EQ
--     compare (x :| xs) (y :| ys) = compare x y <> compare xs ys
