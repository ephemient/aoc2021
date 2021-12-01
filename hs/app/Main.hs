{-# LANGUAGE NondecreasingIndentation #-}
module Main (main) where

import Day1 (day1a, day1b)

import Control.Monad ((<=<), when)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import Paths_aoc2021 (getDataFileName)
import System.Environment (getArgs)
import Text.Read (readMaybe)

getDayInput :: Int -> IO Text
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= TIO.readFile

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