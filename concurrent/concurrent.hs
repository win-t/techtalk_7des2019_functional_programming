module Main where

import Control.Concurrent
import Control.Monad

microInMillis :: Int
microInMillis = 1000

main :: IO ()
main = do

  forkIO $
    forever $ do
      threadDelay $ 1000 * microInMillis
      putStrLn "Hello World 1"

  forkIO $
    forever $ do
      threadDelay $ 700 * microInMillis
      putStrLn "Hello World 2"

  forever $ threadDelay $ 1000 * microInMillis
