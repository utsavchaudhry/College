{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fwarn-tabs #-}

-- DO NOT change the module declaration or export list!
module Art (
  Point,
  Exp(..),
  eval,
  build,

  favoriteGray,
  favoriteColor,

  gradient,
  wavy,
  weird,
  trippyR, trippyG, trippyB
) where

import Data.Data ( Data, Typeable )
import ImageUtils ( RandomDoubles, splitRandomDoubles )

-- ============================================================
-- Expression language
-- ============================================================

-- Extended with three creative constructors:
--   Ripple e1 e2     : ringy pattern from radial distance
--   Thresh a b t e   : piecewise: if a<b then t else e
--   Warp e u v       : domain-warping; evaluate e at (u,v)
--
-- All operators keep outputs in [-1,1].
data Exp
  = X
  | Y
  | Sin Exp              -- sin (pi * e)
  | Cos Exp              -- cos (pi * e)
  | Mul Exp Exp          -- product
  | Avg Exp Exp          -- (e1 + e2)/2
  | Ripple Exp Exp
  | Thresh Exp Exp Exp Exp
  | Warp Exp Exp Exp
  deriving (Eq, Ord, Show, Data, Typeable)

type Point = (Double, Double)

-- ============================================================
-- Evaluation  (guaranteed to stay in [-1,1])
-- ============================================================

eval :: Exp -> Point -> Double
eval X         (x,_) = x
eval Y         (_,y) = y
eval (Sin e)   p     = sin (pi * eval e p)
eval (Cos e)   p     = cos (pi * eval e p)
eval (Mul a b) p     = eval a p * eval b p
eval (Avg a b) p     = (eval a p + eval b p) / 2

eval (Ripple a b) p =
  let ra = eval a p
      rb = eval b p
      r  = sqrt (ra*ra + rb*rb) -- ∈ [0,√2]
  in sin (pi * r)               -- maps back into [-1,1]

eval (Thresh a b t e) p =
  if eval a p < eval b p then eval t p else eval e p

eval (Warp e u v) p =
  let u' = eval u p
      v' = eval v p
  in eval e (u', v')

-- ============================================================
-- Random expression builder
-- ============================================================

-- Split helpers for independent substreams
split2 :: RandomDoubles -> (RandomDoubles, RandomDoubles)
split2 = splitRandomDoubles

split3 :: RandomDoubles -> (RandomDoubles, RandomDoubles, RandomDoubles)
split3 rs =
  let (a, rest) = splitRandomDoubles rs
      (b, c)    = splitRandomDoubles rest
  in (a, b, c)

split4 :: RandomDoubles -> (RandomDoubles, RandomDoubles, RandomDoubles, RandomDoubles)
split4 rs =
  let (a, r1) = splitRandomDoubles rs
      (b, r2) = splitRandomDoubles r1
      (c, d)  = splitRandomDoubles r2
  in (a, b, c, d)

-- Small chance to terminate early so depths vary
pLeaf :: Int -> Double
pLeaf d = 0.05 + 0.20 / (fromIntegral d + 1.0)

-- ============================================================
-- Problem 2: Building random expressions (final, optimized)
-- ============================================================

-- Safe one-value draw from the stream
draw :: RandomDoubles -> (Double, RandomDoubles)
draw []       = error "build/draw: RandomDoubles exhausted"
draw (r:rest) = (r, rest)

-- Depth-aware stop probability:
-- Small near the root, higher near the leaves; still capped.
stopProb :: Int -> Int -> Double
stopProb m d
  | d <= 0    = 1.0
  | otherwise =
      let x = 1 - (fromIntegral d / fromIntegral m)  -- 0 at root → 1 near leaves
          p = 0.05 + 0.35 * x                         -- 5% .. 40%
      in max 0 (min 0.40 p)                           -- conservative cap

chooseLeaf :: Double -> Exp
chooseLeaf r | r < 0.5   = X
             | otherwise = Y

build :: Int -> RandomDoubles -> Exp
build m rs0 = go m m rs0
  where
    go :: Int -> Int -> RandomDoubles -> Exp
    go _ 0 rs =
      let (rLeaf, _) = draw rs
      in chooseLeaf rLeaf

    go m d rs =
      let (rDec, rsA) = draw rs
      in if rDec < stopProb m d
           then
             let (rLeaf, _) = draw rsA
             in chooseLeaf rLeaf
           else
             -- Choose operator first; then split only as needed.
             let (rOp, rs1)  = draw rsA
                 gateTop      = d > (2*m) `div` 3
             in
             if gateTop
               -- TOP 1/3: disable 3- and 4-ary operators to avoid explosive width
               then
                 if      rOp < 0.20
                   then let (sA, _)   = splitRandomDoubles rs1
                        in  Sin (go m (d-1) sA)
                 else if rOp < 0.40
                   then let (sA, _)   = splitRandomDoubles rs1
                        in  Cos (go m (d-1) sA)
                 else if rOp < 0.60
                   then let (sA, sB) = splitRandomDoubles rs1
                        in  Mul (go m (d-1) sA) (go m (d-1) sB)
                 else if rOp < 0.80
                   then let (sA, sB) = splitRandomDoubles rs1
                        in  Avg (go m (d-1) sA) (go m (d-1) sB)
                   else let (sA, sB) = splitRandomDoubles rs1
                        in  Ripple (go m (d-1) sA) (go m (d-1) sB)
               -- LOWER 2/3: allow everything with your original weights
               else
                 if      rOp < 0.15
                   then let (sA, _)   = splitRandomDoubles rs1
                        in  Sin (go m (d-1) sA)
                 else if rOp < 0.30
                   then let (sA, _)   = splitRandomDoubles rs1
                        in  Cos (go m (d-1) sA)
                 else if rOp < 0.48
                   then let (sA, sB) = splitRandomDoubles rs1
                        in  Mul (go m (d-1) sA) (go m (d-1) sB)
                 else if rOp < 0.66
                   then let (sA, sB) = splitRandomDoubles rs1
                        in  Avg (go m (d-1) sA) (go m (d-1) sB)
                 else if rOp < 0.82
                   then let (sA, sB) = splitRandomDoubles rs1
                        in  Ripple (go m (d-1) sA) (go m (d-1) sB)
                 else if rOp < 0.91
                   then let (sE, rest) = splitRandomDoubles rs1
                            (sU, sV)   = splitRandomDoubles rest
                        in  Warp (go m (d-1) sE) (go m (d-1) sU) (go m (d-1) sV)
                   else let (sA, rest1) = splitRandomDoubles rs1
                            (sB, rest2) = splitRandomDoubles rest1
                            (sT, sE)    = splitRandomDoubles rest2
                        in  Thresh (go m (d-1) sA) (go m (d-1) sB)
                                   (go m (d-1) sT) (go m (d-1) sE)



-- ============================================================
-- Favorites
-- ============================================================

favoriteGray  :: (Int, Int)
favoriteGray  = (6, 3)

favoriteColor :: (Int, Int)
favoriteColor = (6, 2)

-- ============================================================
-- Sample expressions
-- ============================================================

gradient :: Exp
gradient = Avg X Y

wavy :: Exp
wavy = Avg (Cos X) (Sin Y)

weird :: Exp
weird = Mul
          (Sin (Sin (Sin (Cos Y))))
          (Cos (Sin (Cos (Avg (Sin Y) (Mul X X)))))

trippyR, trippyG, trippyB :: Exp
trippyR = Ripple (Sin (Mul X Y)) (Avg X (Cos Y))
trippyG = Warp (Sin (Ripple X Y)) (Avg X (Sin Y)) (Avg Y (Cos X))
trippyB = Thresh X Y (Cos (Mul X (Sin Y))) (Sin (Mul Y (Cos X)))
