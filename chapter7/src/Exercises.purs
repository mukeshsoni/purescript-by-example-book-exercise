module Exercises where

import Prelude
import Control.Apply
import Data.AddressBook
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Maybe

liftedAddress = lift3 address (Just "Blah address") Nothing (Just "CA")
liftedAddress2 = lift3 address (Just "Blah address") (Just "Bangalore") (Just "CA")

liftedPlus = lift2 (+)
liftedMinus = lift2 (-)
liftedMul = lift2 (*)
liftedDiv = lift2 (/)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow liftedAddress
  logShow liftedAddress2
  logShow $ liftedPlus (Just 2) (Just 3)
  logShow $ liftedPlus (Just 2) Nothing
  logShow $ liftedMinus (Just 2) (Just 3)
  logShow $ liftedMinus (Just 2) Nothing
  logShow $ liftedMul (Just 2) (Just 3)
  logShow $ liftedMul (Just 2) Nothing
  logShow $ liftedDiv (Just 2) (Just 3)
  logShow $ liftedDiv (Just 2) Nothing