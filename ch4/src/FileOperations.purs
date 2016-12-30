module FileOperations where

import Data.Maybe (Maybe(..))
import Prelude (bind, not, pure, ($), (<), (<<<), (==), (>))
import Control.MonadZero (guard)
import Data.Array (concatMap, filter, foldl, head, length, (:))
import Data.Path (Path, filename, isDirectory, ls, size, root)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- 4.17 Exercise 1
onlyFiles :: Path -> Array Path
onlyFiles file = filter (not isDirectory) (allFiles file)

-- 4.17 Exercise 2
largestFile :: Path -> Path
largestFile file = foldl biggest file (onlyFiles file) where
  biggest :: Path -> Path -> Path
  biggest acc curr =
    if (size curr) > (size acc)
    then curr
    else acc

smallestFile :: Path -> Path
smallestFile file = foldl smallest (largestFile file) (onlyFiles file) where
  smallest :: Path -> Path -> Path
  smallest acc curr =
    if (size acc) > (size curr)
    then curr
    else acc

-- as Maybe
mayLargestFile :: Path -> Maybe Path
mayLargestFile = foldl largest Nothing <<< onlyFiles where
  largest :: Maybe Path -> Path -> Maybe Path
  largest Nothing  x = Just x
  largest (Just y) x = if size x > size y
    then Just x
    else Just y

maySmallestFile :: Path -> Maybe Path
maySmallestFile = foldl smallest Nothing <<< onlyFiles where
  smallest :: Maybe Path -> Path -> Maybe Path
  smallest Nothing  y = Just y
  smallest (Just x) y = if size x > size y
    then Just y
    else Just x

-- 4.17 Exercise 2
eqFilename :: String -> Path -> Boolean
eqFilename name file = name == filename file

whereIs :: String -> Maybe Path
whereIs name = head $ do
  parent <- filter isDirectory (allFiles root)
  guard $ 0 < length (filter (eqFilename name) (ls parent))
  pure parent
