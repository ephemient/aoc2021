{-# LANGUAGE NondecreasingIndentation #-}
module Main (main) where

import Day1 (day1a, day1b)
import Day2 (day2a, day2b)
import Day3 (day3a, day3b)
import Day4 (day4)
import Day5 (day5a, day5b)
import Day6 (day6a, day6b)
import Day7 (day7a, day7b)
import Day8 (day8a, day8b)
import Day9 (day9a, day9b)
import Day10 (day10a, day10b)
import Day11 (day11)
import Day12 (day12a, day12b)
import Day13 (day13a, day13b)
import Day14 (day14a, day14b)
import Day15 (day15a, day15b)

import Control.Monad ((<=<), when)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import Paths_aoc2021 (getDataFileName)
import System.Environment (getArgs)
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent, TraversableStream, VisualStream, errorBundlePretty)
import Text.Read (readMaybe)

getDayInput :: Int -> IO Text
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= TIO.readFile

justOrFail :: (MonadFail m) => Maybe a -> m a
justOrFail = maybe (fail "(⊥)") pure

rightOrFail :: (ShowErrorComponent e, TraversableStream s, VisualStream s, MonadFail m) =>
    Either (ParseErrorBundle s e) a -> m a
rightOrFail = either (fail . errorBundlePretty) pure

run :: Int -> (a -> IO ()) -> [Text -> a] -> IO ()
run day showIO funcs = do
    days <- mapMaybe readMaybe <$> getArgs
    when (null days || day `elem` days) $ do
    putStrLn $ "Day " ++ show day
    contents <- getDayInput day
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    run 1 (print <=< either fail pure) [day1a, day1b]
    run 2 (print <=< either fail pure) [day2a, day2b]
    run 3 (print <=< rightOrFail) [day3a, day3b]
    run 4 (print <=< justOrFail <=< rightOrFail) [day4]
    run 5 (print <=< rightOrFail) [day5a, day5b]
    run 6 (print <=< either fail pure) [day6a, day6b]
    run 7 (print <=< either fail pure) [day7a, day7b]
    run 8 (print <=< justOrFail) [Just . day8a, day8b]
    run 9 print [day9a, day9b]
    run 10 (print <=< justOrFail) [Just . day10a, day10b]
    run 11 (print <=< justOrFail) [day11]
    run 12 (print <=< justOrFail) [day12a, day12b]
    run 13 (mapM_ putStrLn <=< rightOrFail) [fmap ((:[]) . show) . day13a, day13b]
    run 14 (print <=< justOrFail) [day14a, day14b]
    run 15 (print <=< justOrFail) [day15a, day15b]
