module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Math (sqrt, pi)

diagonal w h = sqrt (w * w + h * h)

circleArea r = pi * r * r

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  logShow (diagonal 3.0 4.0)
  logShow (circleArea 4.0)
