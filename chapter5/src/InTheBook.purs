module InTheBook where

import Prelude

gcd :: Int -> Int -> Int
gcd 0 m = m
gcd n 0 = n
gcd n m = if n > m then gcd (n -m) m else gcd n (m - n)
