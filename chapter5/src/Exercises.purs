module Exercises where
  
import Prelude
import Data.Picture

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomialCoefficents :: Int -> Int -> Int
binomialCoefficents n k | k > n = 0
binomialCoefficents _ 0 = 1
binomialCoefficents n r = binomialCoefficents (n - 1) r + binomialCoefficents (n - 1) (r - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address : {city : c1 }} { address : {city : c2 }} = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton a _ = a

origin :: Point
origin = Point {x: 0.0, y: 0.0}

transformShape :: Shape -> Shape
transformShape (Circle _ r) =
    Circle origin (r * 2.0)
transformShape (Rectangle _ w h) =
    Rectangle origin (w * 2.0) (h * 2.0)
transformShape s = s