module Recursion where
import Control.MonadPlus (guard)
import Data.Array (filter, foldl, length, (..))
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, map, pure, ($), (*), (+), (-), (/), (<), (==), (>=), (&&), (<>))

-- 4.4 Exercise 1
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

-- 4.4 Exercise 2
countEven :: Array Int -> Int
countEven [] = 0
countEven xs =
  if isEven $ unsafePartial head xs
    then 1 + countEven (unsafePartial tail xs)
    else 0 + countEven (unsafePartial tail xs)

-- 4.7 Exercise 1
square :: Array Number -> Array Number
square = map (\n -> n * n)

-- 4.7 Exercise 2
removeNegative :: Array Number -> Array Number
removeNegative = filter (\n -> n >= 0.0)

-- 4.7 Exercise 3
infix 8 filter as <$?>

removeNegative' :: Array Number -> Array Number
removeNegative' ns = (\n -> n >= 0.0) <$?> ns

-- 4.11 Exercise 1
factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime 2 = true
isPrime n = if length (factors n) == 1
  then true
  else false

-- 4.11 Exercise 2
cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

-- 4.11 Exercise 3
triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- 1 .. n
  guard $ a*a + b*b < n
  pure [a, b, (a*a + b*b)]

-- 4.11 Exercise 4
factorizations :: Int -> Array Int
factorizations 1 = [1]
factorizations n = do
  f <- 2 .. n
  guard $ isPrime f
  guard $ (n / f) * f == n
  pure f

-- 4.15 Exercise 1
allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

-- 4.15 Exercise 2
-- Any Array with an uneven amount of elements that are false
unevenFalse :: Array Boolean -> Boolean
unevenFalse = foldl (==) false

-- 4.15 Exercise 3
count :: forall a. (a -> Boolean) -> Array a -> Int
count p arr = count' 0 arr
  where
    count' acc [] = acc
    count' acc xs =
      if p (unsafePartial (head xs))
      then count' (acc + 1) (unsafePartial (tail xs))
      else count' acc (unsafePartial (tail xs))

reverse :: forall a. Array a -> Array a
reverse = foldl (\acc curr -> [curr] <> acc) []
