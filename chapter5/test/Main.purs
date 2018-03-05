module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Picture (Point(..), Shape(..), Picture, bounds, showBounds, transformShape, showShape, extractText, area)

circle :: Shape
circle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

rectangle :: Shape
rectangle = Rectangle (Point { x: 10.0, y: 10.0 }) 10.0 10.0

text :: Shape
text = Text (Point {x: 0.0, y: 10.0}) "not cool text"

picture :: Picture
picture = [circle, rectangle]

main :: Eff (console :: CONSOLE) Unit
main = do
    log (showBounds (bounds picture))
    log (showShape (transformShape circle))
    log (showShape (transformShape rectangle))
    logShow (extractText text)
    logShow (extractText circle)
    logShow (map showShape picture)
    logShow picture -- works because we defined show function from Show typeclass for Shape
    logShow (map show picture)
    logShow (area circle)
    logShow (area rectangle)