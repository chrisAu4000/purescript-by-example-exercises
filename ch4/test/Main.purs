module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Recursion (allTrue, count, countEven, factorisations, factors, isEven, isPrime, removeNegative, removeNegative', reverse, square)
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ (isEven 33) == false
  logShow $ (isEven 30) == true
  logShow $ (countEven [1,2,3,4,5,6,7,8,9,10]) == 5
  logShow $ (square [3.0]) == [9.0]
  logShow $ (removeNegative [1.0,-2.0,2.0,-3.0,3.0]) == [1.0,2.0,3.0]
  logShow $ (removeNegative' [1.0,-2.0,2.0,-3.0,3.0]) == [1.0,2.0,3.0]
  logShow $ (factors 16) == [[1,16],[2,8],[4,4]]
  logShow $ (isPrime 31) == true
  logShow $ (isPrime 26) == false
  logShow $ (factorisations 14) == [2,7]
  logShow $ (factorisations 333) == [3,3,37]
  logShow $ (allTrue [true, true, true]) == true
  logShow $ (allTrue [true, true, false]) == false
  logShow $ (count (\b -> b == true) [true, true, false]) == 2
  logShow $ (reverse [3,2,1]) == [1,2,3]
