module TestUtil where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (race)
import Control.Exception (evaluate)
import Data.Maybe (fromMaybe)
import Test.QuickCheck.Property

-- Copied directly from QuickCheck

within :: Testable prop => Int -> prop -> Property
within n = mapRoseResult f
  where
    f rose = ioRose $ do
      let m `orError` x = fmap (fromMaybe x) m
      MkRose res roses <- timeout n (reduceRose rose) `orError`
        return timeoutResult
      res' <- timeout n (protectResult (return $! res)) `orError`
        timeoutResult
      return (MkRose res' (map f roses))

    timeoutResult = failed { reason = "Timeout of " ++ show (fromIntegral n / 1e6 :: Double) ++ " seconds exceeded." }

timeout :: Int -> IO a -> IO (Maybe a)
timeout timeout v = do
  result <-
    race
      (liftIO $ threadDelay timeout)
      v
  case result of
    Left () -> return Nothing
    Right x -> return $ Just x
