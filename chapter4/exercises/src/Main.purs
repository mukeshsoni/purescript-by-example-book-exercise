module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

isEven :: Int -> Boolean
isEven 1 = false
isEven 0 = true
isEven n = isEven (n - 2)

-- count the number of even numbers in an array
countEven :: Array Int -> Int
countEven arr = if null arr then 0 else (ifEvenPlusOne arr) + countEven (unsafePartial tail arr)
                  where
                    ifEvenPlusOne arr =
                      let item = unsafePartial head arr
                      in 
                        if isEven item then 1 else 0


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  logShow $ isEven 3
  logShow $ isEven 6
  logShow $ isEven 0
  logShow $ countEven [1,2,4,5,7,8]
  
