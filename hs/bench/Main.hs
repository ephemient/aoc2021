module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, nf)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as TIO (readFile)
import Day1 (day1a, day1b)
import Paths_aoc2021 (getDataFileName)

getDayInput :: Int -> IO Text
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= TIO.readFile

main :: IO ()
main = defaultMain
  [ env (getDayInput 1) $ \input -> bgroup "Day 1"
      [ bench "part 1" $ nf day1a input
      , bench "part 2" $ nf day1b input
      ]
  ]