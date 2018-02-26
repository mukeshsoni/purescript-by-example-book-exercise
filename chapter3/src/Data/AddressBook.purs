module Data.AddressBook where
  
import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
import Data.Maybe (Maybe, isNothing)

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

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
    where
        filterEntry :: Entry -> Boolean
        filterEntry entry = firstName == entry.firstName && lastName == entry.lastName

containsName :: String -> String -> AddressBook -> Boolean
containsName firstName lastName =
   not <<< isNothing <<< findEntry firstName lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy sameAddress

sameAddress :: Entry -> Entry -> Boolean
sameAddress e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName