module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Data.Foldable hiding (null, length)
import Control.MonadZero (guard)

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

squares :: Array Int -> Array Int
squares = map (\n -> n * n)

removeNegatives :: Array Int -> Array Int
removeNegatives = filter (\n -> n >= 0)

infix 7 filter as <$?>

removeNegatives' :: Array Int -> Array Int
removeNegatives' arr = (\n -> n >= 0) <$?> arr

pairs :: Int -> Array (Array Int)
pairs n = concatMap (\i -> map (\j -> [i, j]) (i..n)) (1..n)

factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) (pairs n)

factors' :: Int -> Array (Array Int)
factors' n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  pure [i, j]

factors'' :: Int -> Array (Array Int)
factors'' n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

-- tells if an integer is prime or not
isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1

cartesian :: Array Int -> Array Int -> Array Int
cartesian arr1 arr2 = do
  i <- arr1
  j <- arr2
  [i * j]

pythagoreanTriples :: Int -> Array (Array Int)
pythagoreanTriples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ (i * i) + (j * j) == k * k
  pure [i, j, k]

allTrue :: Array Boolean -> Boolean
allTrue l = foldl (\acc item -> acc && item) true l

-- Q. Characterize those arrays xs for which the function foldl (==) false xs returns true
-- A. Arrays whose all elements are false

count :: forall a. (a -> Boolean) -> Array a -> Int -> Int
count _ [] n = n
count p xs n = if p (unsafePartial head xs)
                then count p (unsafePartial tail xs) (n + 1)
                else count p (unsafePartial tail xs) n

reverseWithFoldl :: forall a. Array a -> Array a
reverseWithFoldl = foldl (\acc a -> [a] <> acc) []

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  logShow $ isEven 3
  logShow $ isEven 6
  logShow $ isEven 0
  logShow $ countEven [1,2,4,5,7,8]
  logShow $ squares [1,2,4,10]
  logShow $ removeNegatives [1,2,-4,5,3,0,-10]
  logShow $ removeNegatives' [1,2,-4,5,3,0,-10]
  logShow $ pairs 10
  logShow $ factors 20
  logShow $ factors' 20
  logShow $ factors' 20
  logShow $ map isPrime (1..20)
  logShow $ cartesian (1..3) (1..4)
  logShow $ pythagoreanTriples 20
  logShow $ allTrue [true, true, true]
  logShow $ allTrue [true, true, false, true]
  logShow $ count isEven (1..10) 0
  logShow $ count isEven (1..10) 0 == countEven (1..10)
  logShow $ reverseWithFoldl [true, true, false, true]
  logShow $ reverseWithFoldl [1, 2, 3, 4]