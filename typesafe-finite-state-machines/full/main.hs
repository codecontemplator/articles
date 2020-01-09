{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE KindSignatures #-} 

import Prelude hiding (return, (>>=), (>>))
import IxMonadCore
import IxMonadDoNotation
import Data.List.NonEmpty
import Control.Monad.Identity (Identity)
import Control.Monad.IO.Class

-- https://wickstrom.tech/finite-state-machines/2017/11/19/finite-state-machines-part-2.html

data CartItem = CartItem { name :: String }
data Card = Card { uid :: String }
data OrderId = OrderId String

data CheckoutStateTag = NoItemsTag | HasItemsTag | NoCardTag | CardSelectedTag | CardConfirmedTag | OrderPlacedTag

data CheckoutState (s :: CheckoutStateTag) where
  NoItems :: CheckoutState NoItemsTag
  HasItems :: NonEmpty CartItem -> CheckoutState HasItemsTag
  NoCard ::  NonEmpty CartItem -> CheckoutState NoCardTag
  CardSelected :: NonEmpty CartItem -> Card -> CheckoutState CardSelectedTag
  CardConfirmed :: NonEmpty CartItem -> Card -> CheckoutState CardConfirmedTag
  OrderPlaced :: OrderId -> CheckoutState OrderPlacedTag

class SelectState (s :: CheckoutStateTag) where
  addItem :: CartItem -> CheckoutState s -> CheckoutState HasItemsTag

instance SelectState NoItemsTag where
  addItem item _ = HasItems (item :| [])

instance SelectState HasItemsTag where
  addItem item (HasItems items) = HasItems (item <| items)

class CancelState (s :: CheckoutStateTag) where
  getItems :: CheckoutState s -> CheckoutState HasItemsTag

instance CancelState NoCardTag where
  getItems (NoCard items) = HasItems items

instance CancelState CardSelectedTag where
  getItems (CardSelected items _) = HasItems items

instance CancelState CardConfirmedTag where
  getItems (CardConfirmed items _) = HasItems items

type EffectMonad = IO 

select :: SelectState ss => CartItem -> IxStateT EffectMonad (CheckoutState ss) (CheckoutState HasItemsTag) ()
select item = do
  imodify $ addItem item
  return ()

checkout :: IxStateT EffectMonad (CheckoutState HasItemsTag) (CheckoutState NoCardTag) ()
checkout = imodify (\(HasItems items) -> NoCard items) >> return ()

selectCard :: Card -> IxStateT EffectMonad (CheckoutState NoCardTag) (CheckoutState CardSelectedTag) ()
selectCard card = imodify (\(NoCard items) -> CardSelected items card) >> return ()

confirmCard :: IxStateT EffectMonad (CheckoutState CardSelectedTag) (CheckoutState CardConfirmedTag) ()
confirmCard = imodify (\(CardSelected items card) -> CardConfirmed items card) >> return ()

placeOrder ::  IxStateT EffectMonad (CheckoutState CardConfirmedTag) (CheckoutState OrderPlacedTag) OrderId
placeOrder = iput (OrderPlaced oid) >> return oid
  where oid = OrderId "123"

cancelCheckout :: CancelState cs => IxStateT EffectMonad (CheckoutState cs) (CheckoutState HasItemsTag) ()
cancelCheckout = imodify getItems >> return ()


deriving instance Show (CartItem)
deriving instance Show (Card)
deriving instance Show (OrderId)
deriving instance Show (CheckoutState s)

main = runIxStateT checkoutProgram NoItems 
  where 
    getLineWithPrompt msg = do
      ilift $ putStrLn msg
      ilift $ getLine

    selectFirstItem :: IxStateT EffectMonad (CheckoutState NoItemsTag) (CheckoutState HasItemsTag) ()
    selectFirstItem = do
      item <- CartItem <$> getLineWithPrompt "First item:"
      select $ item
      return ()

    selectMoreItems :: IxStateT EffectMonad (CheckoutState HasItemsTag) (CheckoutState HasItemsTag) ()
    selectMoreItems = do
      maybeMore <- (=="y") <$> getLineWithPrompt "More items? (y/N)"
      if maybeMore then do
        ilift $ putStrLn "Next item:"
        item <- ilift $ CartItem <$> getLine
        select $ item
        selectMoreItems
      else
        return ()

    startCheckout :: IxStateT EffectMonad (CheckoutState HasItemsTag) (CheckoutState OrderPlacedTag) OrderId
    startCheckout = do
      checkout
      card <- Card <$> getLineWithPrompt "Card:"
      selectCard card
      confirmed <- (=="y") <$> getLineWithPrompt ("Confirm card " ++ uid card ++ " (y/N)")
      if confirmed then
        confirmCard >> placeOrder
      else
        cancelCheckout >> selectMoreItems >> startCheckout

    fillCart = selectFirstItem >> selectMoreItems

    end (OrderId orderId) = do
      ilift $ putStrLn $ "Completed with order ID: " ++ orderId
      return ()

    checkoutProgram = fillCart >> startCheckout >>= end
