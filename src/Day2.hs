{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Parsing
import qualified Text.Megaparsec as P
import qualified Data.Text as T
data PasswordRule = PasswordRule {range :: [Int]
                                 , character :: Char
                                 , str :: T.Text
                                 } deriving(Show)

file :: FilePath
file = "./src/day2.txt"

rule :: Parser PasswordRule
rule = do
  low <- integer
  dash
  high <- integer
  spaceConsumer
  ch <- lowerChar
  colon
  str <- charseq
  return PasswordRule {range = [low, high], character = ch, str = str}
  
parser :: Parser [PasswordRule]
parser = P.many rule

main :: Int -> IO ()
main opt = do
  rules <- parse parser file
  case rules of
    Right rs -> print $ (length $ validate opt rs)
    Left e -> print e

validate :: Int -> [PasswordRule] -> [PasswordRule]
validate 1 rules = filter count rules
validate 2 rules = filter index rules

index :: PasswordRule -> Bool
index (PasswordRule {range = [low, high], character = c, str = s}) =
  (index == c || index' == c) && (index /= index')
    where
      index = T.index s (low - 1)
      index' = T.index s (high - 1)

count :: PasswordRule -> Bool
count (PasswordRule {range = [low, high], character = c, str = s}) =
  let count = T.count (T.pack [c]) s in
      count >= low && count <= high







