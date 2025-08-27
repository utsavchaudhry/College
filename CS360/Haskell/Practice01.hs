-- Practice01.hs
-- A tiny Haskell practice workbook you can load in GHCi.
-- Open a terminal, then:
--   ghci Practice01.hs
-- In GHCi, try commands like:
--   :t map
--   :i (:)
--   :k Maybe
--   :reload
-- Keep this file open and edit the TODOs; save, then use :reload in GHCi.

module Practice01 where

--------------------------------------------------------------------------------
-- SECTION 0: Warm-up (do these in GHCi; not code)
-- 0.1  What is the type of (:) ?               -- try  :t (:)
-- 0.2  What is the type of map ?               -- try  :t map
-- 0.3  What is the kind of Maybe ?             -- try  :k Maybe
-- 0.4  What is the type of (P) below?          -- after you load, try  :t P
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- SECTION 1: Constructors are functions + partial application
--------------------------------------------------------------------------------

-- We’ll use a 3D point. Notice the data constructor P.
data Point3 a = P a a a
  deriving (Eq, Show)

-- 1.1  In GHCi, ask for P’s type. What is it?
--      Hint: it is a function a -> a -> a -> Point3 a.

-- 1.2  Partially apply P:
--      In GHCi, define  pXY = P 1 2      -- what is :t pXY ?
--      Then do       pXY 3
--      Explain (to yourself) why that works.

-- 1.3  Map a constructor:
--      Evaluate:   map P [1,2] [10,20] [100,200]
--      What result do you get? (Think about map’s type.)

--------------------------------------------------------------------------------
-- SECTION 2: Write small functions on Point3
--------------------------------------------------------------------------------

-- 2.1  origin: the (0,0,0) point (for any numeric type)
origin :: Num a => Point3 a
origin = undefined  -- TODO: replace undefined

-- 2.2  moveX: shift a point in x by dx
moveX :: Num a => a -> Point3 a -> Point3 a
moveX = undefined  -- TODO

-- 2.3  magnitude: Euclidean length of a point-vector
magnitude :: Floating a => Point3 a -> a
magnitude = undefined  -- TODO

-- 2.4  fmap over Point3 (extra): make Point3 a Functor by hand (optional)
--      Uncomment and implement if you want a challenge.
-- instance Functor Point3 where
--   fmap = undefined

--------------------------------------------------------------------------------
-- SECTION 3: Maybe & Either for safe operations
--------------------------------------------------------------------------------

-- 3.1  safeHead: first element if it exists
safeHead :: [a] -> Maybe a
safeHead = undefined  -- TODO (pattern match on [] and (x:_))

-- 3.2  safeDiv: integer division that fails on divide-by-zero
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv = undefined  -- TODO

-- 3.3  parseIntEither: turn a String into an Int, with an error message on fail
--      You may use readEither from Text.Read.
import Text.Read (readEither)

parseIntEither :: String -> Either String Int
parseIntEither = undefined  -- TODO (wrap/transform readEither's Left)

--------------------------------------------------------------------------------
-- SECTION 4: map, filter, foldr – practice with lists
--------------------------------------------------------------------------------

-- 4.1  add3All: add 3 to every element using partial application of (+)
add3All :: Num a => [a] -> [a]
add3All = undefined  -- TODO: map something

-- 4.2  onlyLongerThan: keep only strings longer than n
onlyLongerThan :: Int -> [String] -> [String]
onlyLongerThan = undefined  -- TODO: filter with a lambda or section

-- 4.3  sumSquares: sum of squares using foldr
sumSquares :: Num a => [a] -> a
sumSquares = undefined  -- TODO: foldr

--------------------------------------------------------------------------------
-- SECTION 5: Pattern matching & recursion
--------------------------------------------------------------------------------

-- 5.1  len: your own length
len :: [a] -> Int
len = undefined  -- TODO: follow the (x:xs) pattern

-- 5.2  elem': membership test using recursion (no built-ins)
elem' :: Eq a => a -> [a] -> Bool
elem' = undefined  -- TODO

--------------------------------------------------------------------------------
-- SECTION 6: Quick checks in main (optional to run)
--------------------------------------------------------------------------------

-- Fill your implementations above, then run `main` to spot-check.
-- (Running main before finishing will crash due to undefined.)

tests :: IO ()
tests = do
  putStrLn "Running a few spot checks…"
  -- Uncomment after you implement:
  -- print (origin :: Point3 Int)
  -- print (moveX 5 (P 1 2 3))
  -- print (round (magnitude (P 3 4 12) :: Double))  -- expect 13
  -- print (safeHead ([] :: [Int]))                   -- expect Nothing
  -- print (safeHead [42])                            -- expect Just 42
  -- print (safeDiv 10 2)                             -- expect Just 5
  -- print (safeDiv 1 0)                              -- expect Nothing
  -- print (add3All [1..5])                           -- expect ?
  -- print (onlyLongerThan 3 ["a","abcd","hi","world"])
  -- print (sumSquares [1..5])                        -- expect ?
  -- print (len [True,False,True])
  -- print (elem' 'x' "hex")
  -- print (parseIntEither "123")
  -- print (parseIntEither "abc")

main :: IO ()
main = tests
