module IxMonadDoNotation where

import IxMonadCore
import Prelude hiding ((>>=), (>>), return)

-- override do-notation for usage by indexed monads
return :: (Monad m) => a -> IxStateT m si si a
return = ireturn

(>>=) :: (Monad m) => IxStateT m p q a -> (a -> IxStateT m q r b) -> IxStateT m p r b
(>>=) = ibind

(>>) :: (Monad m) => IxStateT m p q a -> IxStateT m q r b -> IxStateT m p r b
v >> w = v >>= \_ -> w

class IfThenElse b where
  ifThenElse :: b -> a -> a -> a
  
instance IfThenElse Bool where
  ifThenElse True  t _ = t
  ifThenElse False _ f = f