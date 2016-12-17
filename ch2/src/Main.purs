module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Math (pi)

-- Exercise 1

circleArea :: Number -> Number
circleArea r = r * r * pi

-- Exercise 2
-- $ bower install --save purescript-globals

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
