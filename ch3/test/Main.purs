module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.AddressBook (Address, AddressBook, Entry, emptyBook, findEntryByStreet, includesName, insertEntry, removeDuplicates, showEntry)

address :: String -> String -> String -> Address
address street city state =
  { street: street
  , city: city
  , state: state
  }

entry :: String -> String -> Address -> Entry
entry fn ln a =
  { firstName: fn
  , lastName: ln
  , address: a
  }

showBook :: AddressBook -> String
showBook = show <<< map showEntry

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ map showEntry (findEntryByStreet "Teststreet" emptyBook)
  logShow $ map showEntry (findEntryByStreet "Teststreet" (insertEntry (entry "first" "last" $ address "Teststreet" "Testcity" "TE.") emptyBook))
  logShow $ includesName "first" "last" emptyBook
  logShow $ includesName "first" "last" (insertEntry (entry "first" "last" $ address "Teststreet" "Testcity" "TE.") emptyBook)
  logShow $ showBook $ removeDuplicates emptyBook
  logShow $ showBook $ removeDuplicates (insertEntry (entry "first" "last" $ address "Teststreet" "Testcity" "TE.") (insertEntry (entry "first" "last" $ address "Teststreet" "Testcity" "TE.") emptyBook))
