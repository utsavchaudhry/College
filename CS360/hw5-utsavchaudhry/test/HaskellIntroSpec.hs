module HaskellIntroSpec (
    main,
    spec
  ) where

import Test.Hspec
import Test.QuickCheck hiding ( within )

import HaskellIntro
import Util ( within )

-- 1 second, in microseconds (for within)
sec :: Int
sec = 1000000

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Problem 1: Implementing the Luhn Algorithm" $ do
    it "lastDigit 123 == 3" $ do
        lastDigit 123 `shouldBe` 3
    it "lastDigit 0 == 0" $
      lastDigit 0 `shouldBe` 0
    it "dropLastDigit 123 == 12" $
      dropLastDigit 123 `shouldBe` 12
    it "dropLastDigit 5 == 0" $
      dropLastDigit 5 `shouldBe` 0
    it "toDigits 1234 == [1,2,3,4]" $
      toDigits 1234 `shouldBe` [1,2,3,4]
    it "toDigits 0 == []" $
      toDigits 0 `shouldBe` []
    it "toDigits (-17) == []" $
      toDigits (-17) `shouldBe` []
    it "doubleEveryOther [8,7,6,5] == [16,7,12,5]" $
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
    it "doubleEveryOther [1,2,3] == [1,4,3]" $
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
    it "sumDigits [] == 0" $
      sumDigits [] `shouldBe` 0
    it "sumDigits [0] == 0" $
      sumDigits [0] `shouldBe` 0
    it "sumDigits [16,7,12,5] == 22" $
      sumDigits [16,7,12,5] `shouldBe` 22
    it "sumDigits [27] == 9" $
      sumDigits [27] `shouldBe` 9
    it "validate 4012888888881881 = True" $
      validate 4012888888881881 `shouldBe` True
    it "validate 4012888888881882 = False" $
      validate 4012888888881882 `shouldBe` False
  describe "Problem 2: Wobbly Values" $ do
    it "onlyStable" $
        onlyStable [Stable "a", Wobbly "b", Stable "c"] `shouldBe` ["a","c"]
    it "mapWobbly" $
        mapWobbly (++"s") [Stable "cat", Wobbly "dog", Stable "bird"] `shouldBe` [Stable "cats",Wobbly "dogs",Stable "birds"]
    it "splitWobbly" $
        splitWobbly [Stable "cat", Wobbly "dog", Stable "bird"] `shouldBe` (["cat","bird"],["dog"])
  describe "Problem 3: Hofstadter Sequences" $ do
    it "pow square 0" $
      let square :: Integer -> Integer
          square x = x * x
      in
        forAll arbitrary $ \x -> pow square 0 x === x
    it "pow square 2" $
      let square :: Integer -> Integer
          square x = x * x
      in
        forAll arbitrary $ \x -> pow square 2 x === x*x*x*x
    it "G sequence correct" $
      within (10*sec) $
      testOEIS "A005206" (map g [0..])
    it "H sequence correct" $
      within (10*sec) $
      testOEIS "A005374" (map h [0..])
    it "D_2 sequence correct" $
      within (10*sec) $
      testOEIS "A005206" (map (d 2) [0..])
    it "D_3 sequence correct" $
      within (10*sec) $
      testOEIS "A005374" (map (d 3) [0..])
  where
    testOEIS :: String -> [Integer] -> Property
    testOEIS oeid xs =
        take (length ys) xs === ys
      where
        Just ys = getSequenceByID oeid

    -- Since the oeis package has bit-rotted...
    getSequenceByID :: String -> Maybe [Integer]
    -- Hofstadter G-sequence
    getSequenceByID "A005206" = Just [0, 1, 1, 2, 3, 3, 4, 4, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 12, 13, 14, 14, 15, 16, 16, 17, 17, 18, 19, 19, 20, 21, 21, 22, 22, 23, 24, 24, 25, 25, 26, 27, 27, 28, 29, 29, 30, 30, 31, 32, 32, 33, 33, 34, 35, 35, 36, 37, 37, 38, 38, 39, 40, 40, 41, 42, 42, 43, 43, 44, 45, 45, 46, 46, 47]
    -- Hofstadter H-sequence
    getSequenceByID "A005374" = Just [0, 1, 1, 2, 3, 4, 4, 5, 5, 6, 7, 7, 8, 9, 10, 10, 11, 12, 13, 13, 14, 14, 15, 16, 17, 17, 18, 18, 19, 20, 20, 21, 22, 23, 23, 24, 24, 25, 26, 26, 27, 28, 29, 29, 30, 31, 32, 32, 33, 33, 34, 35, 35, 36, 37, 38, 38, 39, 40, 41, 41, 42, 42, 43, 44, 45, 45, 46, 46, 47, 48, 48, 49, 50]
    getSequenceByID _         = Nothing
