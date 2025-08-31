{-# LANGUAGE ScopedTypeVariables #-}

module ArtSpec where

import Codec.Picture
import Control.Monad ( when )
import Data.Data
    ( Data(gunfold, dataTypeOf, toConstr, gmapQ),
      Typeable,
      Constr,
      dataTypeConstrs,
      showConstr,
      typeOf,
      TypeRep )
import Data.Functor.Const ( Const(..) )
import Data.List ( (\\), intercalate, nub )
import Data.Maybe ( catMaybes, fromJust )
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V
import System.Random ( mkStdGen, RandomGen(split) )
import Test.Hspec
import Test.QuickCheck hiding (within)
import Text.Printf ( printf )

import Art ( Exp(X, Y, Sin, Cos, Avg, Mul), Point, build, eval, favoriteGray, favoriteColor )
import GenArt ( imageColor, imageGray, writeRandomGrayToFile, writeRandomColorToFile )
import ImageUtils ( mkRandomDoubles, mkRandomDoubles' )
import Paths_art ( getDataFileName )
import TestUtil (within)

-- 1 second, in microseconds (for within)
sec :: Int
sec = 1000000

newtype Seed = Seed { getSeed :: Int }
  deriving (Eq, Ord, Show)

instance Arbitrary Seed where
  arbitrary = Seed <$> arbitrary

  shrink _ = []

newtype Depth = Depth { getDepth :: Int }
  deriving (Eq, Ord, Show)

instance Arbitrary Depth where
  arbitrary = do NonNegative d <- arbitrary
                 pure $ Depth d

  shrink (Depth 0) = []
  shrink (Depth d) = [Depth d' | NonNegative d' <- shrink (NonNegative d)]

spec :: Spec
spec = do
  describe "Problem 1: Expression Evaluation" $ do
    evalSpec
  describe "Problem 2: Building Random Expressions" $ do
    buildSpec
  describe "Problem 3: Extending the Expression Language" $ do
    extendSpec
  describe "Problem 4: Submit Your Favorite Image" $ do
    favoriteSpec

evalSpec :: Spec
evalSpec = do
    compareGray "gradient.png" gradient
    compareGray "wavy.png" wavy
    compareGray "weird.png" weird
    compareColor "trippy.png" trippyR trippyG trippyB
  where
    compareImageWithSample :: String -> DynamicImage -> Spec
    compareImageWithSample name image' = do
      maybe_image <- runIO $ getDataFileName ("samples/" <> name) >>= readPng
      it ("Compare with " ++ name) $
        case maybe_image of
          Left err    -> expectationFailure err
          Right image -> let golden = convertRGB8 image
                             user = convertRGB8 image'
                             diff = V.zipWith (\x y -> (x-y)^2) (imageData golden) (imageData user)
                         in
                           when (V.maximum diff > 1) $
                             expectationFailure "Generated image does not match sample"

    compareGray :: String -> Exp -> Spec
    compareGray name e =
      compareImageWithSample name (ImageY8 (imageGray 128 e))

    compareColor :: String -> Exp -> Exp -> Exp -> Spec
    compareColor name er eg eb =
      compareImageWithSample name (ImageRGB8 (imageColor 128 er eg eb))

    gradient :: Exp
    gradient = Avg X Y

    wavy :: Exp
    wavy = Avg (Cos X) (Sin Y)

    weird :: Exp
    weird = Mul (Sin (Sin (Sin (Sin (Sin (Sin (Sin (Cos Y))))))))
                (Cos (Sin (Cos (Avg (Sin Y) (Mul X X)))))

    trippyR, trippyG, trippyB :: Exp
    trippyR = Sin (Avg (Mul (Mul (Mul (Cos (Mul (Sin (Cos Y)) (Avg (Avg X X) (Sin Y)))) (Avg (Sin (Mul (Sin Y) (Mul Y X))) (Cos (Cos (Mul Y Y))))) (Sin (Mul (Sin (Mul (Sin Y) (Sin Y))) (Cos (Mul (Mul Y Y) (Sin Y)))))) (Sin (Avg (Cos (Avg (Mul (Mul Y X) (Mul X X)) (Sin (Mul Y X)))) (Sin (Avg (Avg (Sin X) (Avg X X)) (Sin (Avg X Y))))))) (Cos (Cos (Avg (Sin (Sin (Avg (Mul X X) (Mul X X)))) (Sin (Sin (Sin (Sin Y))))))))

    trippyG = Sin (Mul (Mul (Avg (Avg (Cos (Mul (Cos (Cos X)) (Mul (Cos X) (Avg Y X)))) (Mul (Mul (Cos (Cos Y)) (Mul (Cos X) (Mul X Y))) (Sin (Sin (Avg Y Y))))) (Cos (Mul (Avg (Sin (Sin X)) (Sin (Sin X))) (Sin (Sin (Mul X Y)))))) (Avg (Mul (Avg (Cos (Sin (Cos X))) (Avg (Mul (Sin X) (Cos Y)) (Avg (Cos X) (Cos X)))) (Avg (Avg (Sin (Cos X)) (Sin (Sin X))) (Mul (Avg (Cos X) (Avg Y X)) (Avg (Sin Y) (Sin X))))) (Mul (Cos (Cos (Mul (Avg Y Y) (Mul Y X)))) (Cos (Cos (Sin (Avg X X))))))) (Sin (Avg (Avg (Sin (Cos (Sin (Cos X)))) (Avg (Sin (Cos (Cos Y))) (Mul (Mul (Sin Y) (Mul X Y)) (Cos (Mul Y Y))))) (Cos (Avg (Mul (Mul (Cos Y) (Mul Y Y)) (Avg (Sin Y) (Cos Y))) (Mul (Mul (Mul X X) (Avg Y X)) (Cos (Sin X))))))))

    trippyB = Avg (Sin (Mul (Avg (Cos (Avg (Mul (Cos (Mul X X)) (Cos (Mul X Y))) (Avg (Avg (Mul X X) (Avg Y Y)) (Avg (Cos Y) (Cos X))))) (Avg (Avg (Avg (Mul (Sin Y) (Mul X Y)) (Sin (Mul X X))) (Avg (Mul (Mul X X) (Sin Y)) (Mul (Avg X X) (Sin Y)))) (Avg (Mul (Cos (Sin Y)) (Cos (Avg X X))) (Sin (Avg (Sin Y) (Sin Y)))))) (Cos (Avg (Avg (Avg (Sin (Mul X X)) (Avg (Sin Y) (Sin X))) (Cos (Avg (Cos Y) (Avg Y X)))) (Mul (Mul (Mul (Avg X Y) (Cos X)) (Cos (Avg Y X))) (Avg (Cos (Mul Y X)) (Mul (Mul X X) (Mul Y X)))))))) (Avg (Mul (Mul (Mul (Mul (Sin (Sin (Avg X X))) (Avg (Avg (Sin Y) (Sin Y)) (Avg (Avg X X) (Cos Y)))) (Sin (Sin (Sin (Mul Y Y))))) (Avg (Cos (Avg (Avg (Avg X Y) (Mul Y X)) (Cos (Sin X)))) (Mul (Sin (Sin (Sin X))) (Cos (Mul (Mul Y Y) (Cos X)))))) (Avg (Cos (Cos (Sin (Cos (Avg X Y))))) (Mul (Sin (Mul (Cos (Avg Y X)) (Sin (Cos X)))) (Mul (Mul (Sin (Cos Y)) (Avg (Avg X X) (Cos X))) (Avg (Mul (Sin X) (Avg Y X)) (Sin (Sin X))))))) (Mul (Mul (Cos (Cos (Mul (Sin (Mul Y Y)) (Cos (Cos X))))) (Avg (Sin (Avg (Cos (Sin Y)) (Mul (Cos X) (Avg X X)))) (Cos (Cos (Cos (Avg X Y)))))) (Sin (Mul (Avg (Mul (Cos (Mul Y Y)) (Cos (Sin Y))) (Avg (Mul (Mul X X) (Sin X)) (Cos (Sin Y)))) (Avg (Sin (Mul (Avg Y X) (Avg X X))) (Cos (Avg (Mul Y Y) (Avg Y Y))))))))

buildSpec :: Spec
buildSpec = do
    it "Different seeds produce different expressions" $
      property prop_diff_seeds_differ
    it "Depth of generated expression never exceeds depth parameter" $
      property prop_depth_not_exceeded
    it "Average depth is at least 2/3 of depth parameter" $
      property prop_average_depth
  where
    newFrac :: Rational
    newFrac = 0.75

    requiredAvgDepth :: Rational
    requiredAvgDepth = 2/3

    prop_diff_seeds_differ :: Depth -> Seed -> Property
    prop_diff_seeds_differ (Depth depth) (Seed seed) =
        depth < 12 ==>
        within (10*sec) $
        let count         = 100
            es            = buildMany depth seed count
            uniqueCount   = Set.size (Set.fromList es)
            expectedCount = min (2^depth) (0.75 * fromIntegral count)
        in
          counterexample (show es) $
          fromIntegral uniqueCount >= expectedCount

    prop_depth_not_exceeded :: Depth -> Seed -> Property
    prop_depth_not_exceeded (Depth depth) (Seed seed) =
        depth < 12 ==>
        counterexample (show e) $
        within (1*sec) $
        fromJust (expDepth e) <= depth
      where
        e :: Exp
        e = build depth (mkRandomDoubles seed)

    prop_average_depth :: Depth -> Seed -> Property
    prop_average_depth (Depth depth) (Seed seed) =
        depth < 10 ==>
        within (30*sec) $
        let count    = 100
            es       = buildMany depth seed count
            avgDepth = average (catMaybes (map expDepth es))
        in
          counterexample ("Average depth with depth parameter of " ++ show depth ++ " is " ++ show (fromRational avgDepth :: Double)) $
          avgDepth >= requiredAvgDepth * fromIntegral depth

extendSpec :: Spec
extendSpec = do
    it "Add at least three new expression types" $
      when (length newConstrs < 3) $
        expectedButGot newConstrs
    it "At least one constructor takes at least 3 subexpressions of type Exp" $
      let maxSubexpCount = maximum (map countExpArgs (gallConstrs (undefined :: Exp)))
      in
        when (maxSubexpCount < 3) $
          expectationFailure $
          "Expected a constructor taking at least 3 subexpressions but saw a maximum of " ++ show maxSubexpCount
    it "Use all new expression types" $
      let es = buildMany 5 seed 10
          usedConstrs = map show $ nub $ concatMap gallConstrs es
          newUsedConstrs = usedConstrs \\ givenConstrs
      in
        when (length newUsedConstrs < 3) $
          expectedButGot newUsedConstrs
  where
    seed :: Int
    seed = 100

    givenConstrs, allConstrs, newConstrs :: [String]
    givenConstrs = ["X", "Y", "Sin", "Cos", "Mul", "Avg"]
    allConstrs   = map showConstr (gallConstrs (undefined :: Exp))
    newConstrs   = allConstrs \\ givenConstrs

    expectedButGot :: [String] -> Expectation
    expectedButGot got =
      expectationFailure $
      "Expected at least three new expression types but got" ++
      if null got then " none" else (": " ++ intercalate ", " got)

favoriteSpec :: Spec
favoriteSpec = do
    it "Tell us your favorite grayscale image" $
      let (seed, depth) = favoriteGray
          size = 300
          file = printf "favgray_%d_%d_%d.png" size depth seed
      in
        writeRandomGrayToFile size seed depth file `shouldReturn` ()
    it "Tell us your favorite color image" $
      let (seed, depth) = favoriteColor
          size = 300
          file = printf "favcolor_%d_%d_%d.png" size depth seed
      in
        writeRandomColorToFile size seed depth file `shouldReturn` ()


-- | Determine all data constructors for type `a`
gallConstrs :: forall a. Data a
            => a
            -> [Constr]
gallConstrs = dataTypeConstrs . dataTypeOf

-- | Determine all constructors used in a value
gusedConstrs :: forall a. Data a
             => a
             -> [Constr]
gusedConstrs x = nub $ toConstr x : concat (gmapQ gusedConstrs x)

-- | Calculate depth of values of type 'Exp'
expDepth :: forall a. Data a
         => a
         -> Maybe Int
expDepth x
  | typeOf x == expRep = case catMaybes (gmapQ expDepth x) of
                           [] -> Just 0
                           ds -> Just $ 1 + maximum ds
  | otherwise          = Nothing
  where
    expRep :: TypeRep
    expRep = typeOf (undefined :: Exp)

-- | Count constructor arguments of type 'Exp'
countExpArgs :: Constr -- ^ Constructor
             -> Int    -- ^ Number of arguments of type 'Exp'
countExpArgs = getConst . go
  where
    go :: Constr -> Const Int Exp
    go = gunfold inc (\_ -> Const 0)

    inc :: forall b r . Typeable b => Const Int (b -> r) -> Const Int r
    inc (Const x)
      | typeOf (undefined :: b) == expRep = Const (x+1)
      | otherwise                         = Const x

    expRep :: TypeRep
    expRep = typeOf (undefined :: Exp)

-- | Calculate average of `Real` numbers.
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / fromIntegral (length xs)

-- | Build many expressions
buildMany :: Int   -- ^ Depth
          -> Int   -- ^ Seed
          -> Int   -- ^ Number of expressions to generate
          -> [Exp] -- ^ Generated expressions
buildMany depth seed n0 =
    go (mkStdGen seed) n0
  where
    go :: RandomGen g => g -> Int -> [Exp]
    go _ 0 = []
    go g n = build depth (mkRandomDoubles' g') : go g'' (n-1)
      where
        (g', g'') = split g
