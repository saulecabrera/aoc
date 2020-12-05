{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day1 where

import Parsing
import qualified Text.Megaparsec as P
import Data.Maybe

file :: FilePath
file = "./src/day1.txt"

parser :: Parser [Int]
parser = do
  result <- P.many integer
  return result

process :: Int -> Int -> [Int] -> Maybe Int
process _ _ [] = Nothing
process target 2 (x:xs)
  | result `elem` xs = Just $ result * x
  | otherwise = process target 2 xs
  where result = target - x

process target 3 (x:xs)
  | isNothing result = process target 3 xs
  | isJust result = Just $ x * (fromMaybe 0 result)
  where result = process (target - x) 2 xs

main :: Int -> IO ()
main elems = do
  result <- parse parser file
  case result of
    Right values -> case process 2020 elems values of
                      Just v -> print v
                      Nothing -> print "Couldn't find any matching value"
    Left _ -> print "Couldn't parse input"
