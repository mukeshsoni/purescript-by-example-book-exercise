module FileOperations where
  
import Prelude

import Data.Array (head, filter, concatMap, (:))
import Data.Path (Path(..), ls, isDirectory, size, filename, root)
import Data.Foldable
import Data.Maybe
import Control.MonadPlus

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles path = filter isFile (allFiles path)
    where
        isFile = not <<< isDirectory

sizeFromMaybe :: Maybe Int -> Int
sizeFromMaybe Nothing = 0
sizeFromMaybe (Just x) = x

largestFile :: Path -> Maybe Path
largestFile path = foldl largerFile Nothing (onlyFiles path)
    where
        largerFile Nothing file = Just file
        largerFile (Just f1) f2 = if sizeFromMaybe (size f2) > sizeFromMaybe (size f1) then Just f2 else Just f1

smallestFile :: Path -> Maybe Path
smallestFile path = foldl smallerFile Nothing (onlyFiles path)
    where
        smallerFile Nothing file = Just file
        smallerFile (Just f1) f2 = if sizeFromMaybe (size f2) < sizeFromMaybe (size f1) then Just f2 else Just f1

whereIs :: String -> Maybe Path
whereIs searchString = head do
    dir <- allFiles root
    guard $ isDirectory dir
    file <- ls dir
    guard $ filename file == searchString
    pure dir