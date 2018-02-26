module Data.AddressBook where
  
import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

type Entry = 
    { firstName :: String
    , lastName :: String
    , address :: Address
    }

type Address = 
    { street :: String
    , city :: String
    , state :: String
    }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.firstName <> ", " <>
                  entry.lastName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress address = address.street <> ", " <> address.city <> ", " <> address.state

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

emptyBook :: AddressBook
emptyBook = empty
