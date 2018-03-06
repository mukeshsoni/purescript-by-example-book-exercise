module Data.Hashable where
  
import Prelude
import Data.Function
import Data.Char
import Data.String
import Data.Foldable (foldl)
import Data.Array (nubBy, length)

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode n = HashCode (n `mod` 65535)

class Eq a <= Hashable a where
    hash :: a -> HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = HashCode (73 * h1 + 51 * h2)

instance eqHashCode :: Eq HashCode where
    eq (HashCode h1) (HashCode h2) = h1 == h2

instance showHashCode :: Show HashCode where
    show (HashCode h) = "HashCode " <> show h

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashInt :: Hashable Int where
    hash = hashCode

instance hashBool :: Hashable Boolean where
    hash false = HashCode 0
    hash true = HashCode 1

instance hashChar :: Hashable Char where
    hash = hash <<< toCharCode

instance hashArray :: Hashable a => Hashable (Array a) where
    hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashString :: Hashable String where
    hash = hash <<< toCharArray

-- (Medium) Use the hashEqual function to write a function which tests if an array
-- has any duplicate elements, using hash-equality as an approximation to value equality.
-- Remember to check for value equality using == if a duplicate pair is found.
-- Hint: the nubBy function in Data.Array should make this task much simpler.
hasDuplicates :: forall a. Hashable a => Array a -> Boolean
hasDuplicates arr = (length $ nubBy areEqual arr) /= length arr
    where
        areEqual a b = hash a == hash b && a == b