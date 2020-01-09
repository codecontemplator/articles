module IxMonadCore where
    
class IxMonad m where
    ireturn :: a -> m p p a
    ibind :: m p q a -> (a -> m q r b) -> m p r b

newtype IxStateT m si so v = IxStateT { runIxStateT :: si -> m (so,v) }

instance Monad m => IxMonad (IxStateT m) where
  ireturn x = IxStateT (\si -> return (si,x))
  ibind (IxStateT m) f = IxStateT (\si -> m si >>= (\ (sm,x) -> runIxStateT (f x) sm))    

{-
-- allow for ordinary monads to be index monads
newtype MW m p q a = MW { unMW:: m a }

instance Monad m => IxMonad (MW m) where
    ireturn = MW . return
    ibind (MW m) f = MW (m >>= unMW . f)
-}

{-
-- standard get/put for reference
iget :: Monad m => IxStateT m si si si
iget = IxStateT (\si -> return (si,si))

iput :: Monad m => so -> IxStateT m si so ()
iput x = IxStateT (\si -> return (x,()))
-}

imodify :: Monad m => (si -> so) -> IxStateT m si so ()
imodify f = IxStateT (\si -> return (f si,()))

class IxMonadTrans t where
    ilift :: Monad m => m a -> t m i i a

instance IxMonadTrans IxStateT where
    ilift m = IxStateT $ \s -> m >>= \x -> return (s, x)
