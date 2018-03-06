module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

import Data.Hashable

h1 = Hour 10
h2 = Hour 22
h3 = Hour 5

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow (hash 123)
  logShow (hash true)
  logShow (hash [1, 2, 3])
  logShow (hash "testing")
  logShow (hash 'a')
  logShow ("foo" `hashEqual` "foo")
  logShow ("foo" `hashEqual` "bar")
  logShow (hash h1 == hash h2 && h1 == h2)
  logShow (hash h1 /= hash h3 && h1 /= h3)

