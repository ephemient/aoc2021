{-# LANGUAGE NondecreasingIndentation #-}
module Main (main) where

import Day1 (day1a, day1b)
import Day2 (day2a, day2b)
import Day3 (day3a, day3b)
import Day4 (day4a, day4b)
import Day5 (day5a, day5b)

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
justOrFail = maybe (fail "(⊥)") return

rightOrFail :: (ShowErrorComponent e, TraversableStream s, VisualStream s, MonadFail m) =>
    Either (ParseErrorBundle s e) a -> m a
rightOrFail = either (fail . errorBundlePretty) return

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
    run 4 (print <=< justOrFail <=< rightOrFail) [day4a, day4b]
    run 5 (print <=< rightOrFail) [day5a, day5b]
