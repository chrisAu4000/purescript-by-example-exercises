module Data.AddressBook where
import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)
import Prelude ((&&), (<<<), (<>), (==))

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress address = address.street <> ", " <>
                      address.city <> ", " <>
                      address.state

showEntry :: Entry -> String
showEntry entry = entry.firstName <> ", " <>
                  entry.lastName <> ": " <>
                  showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

equalName :: String -> String -> Entry -> Boolean
equalName firstName lastName entry
  = firstName == entry.firstName && lastName == entry.lastName

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry where
  filterEntry = equalName firstName lastName

-- Exercise 1
-- head :: AddressBook -> Maybe Entry
-- filterEntry :: Entry -> Boolean
-- filter :: (a -> Boolean) -> AddressBook -> AddressBook
--             ---------------filter------------    -----------head-----------
-- findEntry : (Entry -> Boolean) -> AddressBook -> AddressBook -> Maybe Entry
--             ---filterEntry---

-- Exercise 2
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterStreet where
  filterStreet :: Entry -> Boolean
  filterStreet entry = street == entry.address.street

-- Exercise 3
includesName :: String -> String -> AddressBook -> Boolean
includesName firstName lastName = null <<< filter filterEntry where
  filterEntry = equalName firstName lastName

-- Exercise 4
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy notEqualNames where
  notEqualNames :: Entry -> Entry -> Boolean
  notEqualNames e1 = equalName e1.firstName e1.lastName
