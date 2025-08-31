{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

-- You can start GHCi at the UNIX prompt with the command `stack ghci`.
--
-- You can also load the file into GHCi after starting it by typing `:load
-- HaskellIntro.hs` once GHCi has started.
--
-- You can reload a file in GHCi after making changes by typing `:reload`.
--
-- Load this file into GHCi and type `isThisWorking` at the prompt. GHCi will
-- tell you whether it's working!
--

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit n = abs n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = abs n `div` 10

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  let go = zipWith (*) (cycle [1,2])
  in  reverse (go (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate n =
  let s = sumDigits (doubleEveryOther (toDigits n))
  in  s `mod` 10 == 0

--
-- Problem 2
--

data Wobbly a = Stable a | Wobbly a
  deriving (Eq, Ord, Show)

onlyStable :: [Wobbly a] -> [a]
onlyStable xs = [x | Stable x <- xs]

mapWobbly :: (a -> b) -> [Wobbly a] -> [Wobbly b]
mapWobbly f = map step
  where
    step (Stable  x) = Stable  (f x)
    step (Wobbly x) = Wobbly (f x)

splitWobbly :: [Wobbly a] -> ([a], [a])
splitWobbly xs = ([x | Stable  x <- xs],
                  [y | Wobbly y <- xs])


--
-- Problem 3
--

pow :: (a -> a) -> Int -> a -> a
pow _ n | n <= 0 = id             
pow f n          = f . pow f (n-1)

g :: Integer -> Integer
g 0 = 0
g n = n - pow g 2 (n - 1)

-- H(0) = 0
-- H(n) = n - H(H(H(n-1)))  for n>0
h :: Integer -> Integer
h 0 = 0
h n = n - pow h 3 (n - 1)

d :: Int -> Integer -> Integer
d i 0 = 0
d i n = n - pow (d i) i (n - 1)

