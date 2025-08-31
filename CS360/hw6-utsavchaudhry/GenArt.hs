{-# OPTIONS_GHC -fwarn-tabs #-}

module GenArt (
  writeRandomGray,
  writeRandomGrayToFile,
  imageGray,

  writeRandomColor,
  writeRandomColorToFile,
  imageColor,

  writeTestFiles
) where

import Codec.Picture
    ( Image,
      Pixel8,
      writePng,
      PngSavable,
      PixelRGB8(..) )
import System.Random ( StdGen, RandomGen(split), mkStdGen )
import Text.Printf ( printf )

import Art
    ( Point,
      Exp,
      eval,
      build,
      gradient,
      wavy,
      weird,
      trippyR,
      trippyG,
      trippyB )
import ImageUtils
    ( mkImage,
      mkRandomDoubles,
      mkRandomDoubles',
      toPixel8,
      toPos,
      RandomDoubles )

--
-- Support for generating grayscale images
--

writeRandomGray :: Int -> Int -> Int -> IO ()
writeRandomGray size seed depth =
    writeRandomGrayToFile size seed depth file
  where
    file = printf "gray_%d_%d_%d.png" size depth seed

writeRandomGrayToFile :: Int -> Int -> Int -> FilePath -> IO ()
writeRandomGrayToFile size seed depth file =
    writeGray file size e
  where
    rs :: RandomDoubles
    rs = mkRandomDoubles seed

    e :: Exp
    e = build depth rs

writeGray :: FilePath -> Int -> Exp -> IO ()
writeGray file size e = writeImg file $ imageGray size e

imageGray :: Int -> Exp -> Image Pixel8
imageGray n e = mkImage n (mkPixel8 n e)

--
-- Support for generating color images
--

writeRandomColor :: Int -> Int -> Int -> IO ()
writeRandomColor size seed depth =
    writeRandomColorToFile size seed depth file
  where
    file :: String
    file = printf "color_%d_%d_%d.png" size depth seed

writeRandomColorToFile :: Int -> Int -> Int -> FilePath -> IO ()
writeRandomColorToFile size seed depth file =
    writeColor file size er eg eb
  where
    g, g', gr, gg, gb :: StdGen
    g = mkStdGen seed
    (gr, g') = split g
    (gg, gb) = split g'

    rrs, rgs, rbs :: RandomDoubles
    rrs = mkRandomDoubles' gr
    rgs = mkRandomDoubles' gg
    rbs = mkRandomDoubles' gb

    er, eg, eb :: Exp
    er = build depth rrs
    eg = build depth rgs
    eb = build depth rbs

writeColor :: FilePath -> Int -> Exp -> Exp -> Exp -> IO ()
writeColor file size er eg eb = writeImg file $ imageColor size er eg eb

imageColor :: Int -> Exp -> Exp -> Exp -> Image PixelRGB8
imageColor n er eg eb = mkImage n (mkPixelRGB8 n er eg eb)

--
-- Support for generating color images
--

writeImg :: PngSavable a => FilePath -> Image a -> IO ()
writeImg file = writePng ("img/" ++ file)

mkPixel8 :: Int -> Exp -> Int -> Int -> Pixel8
mkPixel8 n e x y = toPixel8 $ checkedEval e (toPos n x, toPos n y)

mkPixelRGB8 :: Int -> Exp -> Exp -> Exp -> Int -> Int -> PixelRGB8
mkPixelRGB8 n er eg eb x y = PixelRGB8 (mkPixel8 n er x y)
                                       (mkPixel8 n eg x y)
                                       (mkPixel8 n eb x y)

checkedEval :: Exp -> Point -> Double
checkedEval e p@(x, y)
    | v < -1.0 || v > 1.0 = error $ "eval: value out of bounds at (" ++ show x ++ "," ++ show y ++ "): " ++ show e
    | otherwise           = v
  where
    v = eval e p

writeTestFiles :: IO ()
writeTestFiles = do
  writeGray "gradient.png" 128 gradient
  writeGray "wavy.png" 128 wavy
  writeGray "weird.png" 128 weird
  writeColor "trippy.png" 128 trippyR trippyG trippyB
