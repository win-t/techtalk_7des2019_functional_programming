module Main where

import Data.List
import Data.Ord

data Score = Score
  { value :: Int
  , name :: String
  }

printScore :: Score -> IO ()
printScore s = putStrLn $ name s ++ " " ++ show (value s)

getMinInList :: [Score] -> Score
getMinInList = head . sortBy (comparing value)

getFirstInList :: [Score] -> Score
getFirstInList = head

getLastInList :: [Score] -> Score
getLastInList = last

main :: IO ()
main = do
  let scores =
        [ Score{name="Alice", value=70}
        , Score{name="Bob", value=60}
        , Score{name="Carlie",value=80}
        ]

  printScore $ getMinInList scores
  printScore $ getFirstInList scores
  printScore $ getLastInList scores
