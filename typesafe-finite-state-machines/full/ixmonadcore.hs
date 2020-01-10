{-# LANGUAGE FlexibleInstances #-}

module IxMonadCore where

import Control.Monad.IO.Class

class IxMonad m where
    ireturn :: a -> m p p a
    ibind :: m p q a -> (a -> m q r b) -> m p r b

newtype IxStateT m si so v = IxStateT { runIxStateT :: si -> m (so,v) }

class IxMonadTrans t where
    ilift :: Monad m => m a -> t m i i a

instance IxMonadTrans IxStateT where
    ilift m = IxStateT $ \s -> m >>= \x -> return (s, x)

instance Monad m => IxMonad (IxStateT m) where
  ireturn x = IxStateT (\si -> return (si,x))
  ibind (IxStateT m) f = IxStateT (\si -> m si >>= (\ (sm,x) -> runIxStateT (f x) sm))    

instance IxMonad m => Monad (m j j) where
    return = ireturn
    (>>=)  = ibind

instance IxMonad m => Functor (m j k) where
    fmap f m = m `ibind` (\a -> ireturn (f a))

instance IxMonad m => Applicative (m j j) where
    pure  = ireturn
    fab <*> fa = fab `ibind` (\ab -> fa `ibind` (\a -> ireturn (ab a)))

instance MonadIO m => MonadIO (IxStateT m p p) where
    liftIO = ilift . liftIO

iget :: Monad m => IxStateT m si si si
iget = IxStateT (\si -> return (si,si))

iput :: Monad m => so -> IxStateT m si so ()
iput x = IxStateT (\si -> return (x,()))

imodify :: Monad m => (si -> so) -> IxStateT m si so ()
imodify f = IxStateT (\si -> return (f si,()))

