module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Recursion (isEven, countEven)
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ (isEven 33) == false
  logShow $ (isEven 30) == true
  logShow $ (countEven [1,2,3,4,5,6,7,8,9,10]) == 5
  
