module Chapter06Cont where

newtype Cont a = Cont
    { unCont :: forall r. (a -> r) -> r
    }

instance Functor Cont where
    fmap :: (a -> b) -> Cont a -> Cont b
    -- if you pass ca a function a -> r, it will return an r
    fmap ab (Cont arr) = Cont $ \brr -> arr (\a -> brr $ ab a)

instance Applicative Cont where
    pure a = Cont $ \arr -> arr a

    -- These aren't really callbacks. It's easier to think of them as values
    -- (<*>) :: Cont (a -> b) -> Cont a -> Cont b
    -- Cont fab <*> Cont ca = Cont $ \cb -> cb $ fab (\ab -> ca (\a -> ab a))

    (<*>) :: Cont (a -> b) -> Cont a -> Cont b
    Cont fab <*> Cont ca =
        -- The implementation of r decides what it is
        Cont $ \cb ->
            fab $ \ab ->
                ca $ \a ->
                    cb (ab a)

instance Monad Cont where
    (>>=) :: Cont a -> (a -> Cont b) -> Cont b
    Cont fa >>= mab = Cont $ \cb ->
        fa $ \a ->
            unCont (mab a) $ \b ->
                cb b
