module Exercises where

import Prelude
import Data.Foldable

data Complex = Complex
    { real :: Number
    , imaginary :: Number
    }

instance showComplex :: Show Complex where
    show (Complex {real, imaginary}) = "Complex [real: " <> show real <> ", imaginary: " <> show imaginary <> "]"

instance eqComplex :: Eq Complex where
    eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
    show (NonEmpty a arr) = "NonEmpty " <> show a <> " " <> show arr

-- 1. Write an Eq instance for the type `NonEmpty a` which reuses the instances for `Eq a` and Eq (Array a)
instance eqNonEmpty :: (Eq a, Eq (Array a)) => Eq (NonEmpty a) where
    eq (NonEmpty a1 arr1) (NonEmpty a2 arr2) = a1 == a2 && arr1 == arr2

-- 2. Write a Semigroup instance for `NonEmpty a` by reusing the Semigroup instance for `Array`
instance appendNonEmpty :: (Semigroup (Array a)) => Semigroup (NonEmpty a) where
    append (NonEmpty a1 arr1) (NonEmpty a2 arr2) = NonEmpty a1 (arr1 <> [a2] <> arr2)


-- 3. Write a functor instance for `NonEmpty`
instance mapNonEmpty :: Functor NonEmpty where
    map f (NonEmpty a arr) = NonEmpty (f a) (map f arr)

-- Given any type a with an instance of Ord, we can add a new “infinite” value which is greater than any other value
data Extended a = Finite a | Infinite

-- 4. Write an Ord instance for Extended a which reuses the Ord instance for a
instance eqExtended :: (Eq a) => Eq (Extended a) where
    eq Infinite _ = false
    eq _ Infinite = false
    eq (Finite a1) (Finite a2) = eq a1 a2

instance compareExtended :: (Ord a) => Ord (Extended a) where
    compare Infinite _ = GT
    compare _ Infinite = LT
    compare (Finite a1) (Finite a2) = compare a1 a2

-- 5. (Difficult) Write a Foldable instance for NonEmpty. Hint: reuse the Foldable instance for arrays
instance foldNonEmpty :: Foldable NonEmpty where
    foldr f acc (NonEmpty a arr) = foldr f (f a acc) arr
    foldl f acc (NonEmpty a arr) = foldl f (f acc a) arr
    foldMap fm (NonEmpty a arr) = foldMap fm ([a] <> arr)