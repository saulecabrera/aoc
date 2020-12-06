{-# LANGUAGE OverloadedStrings #-}

module Parsing
  (
   spaceConsumer
  , lexeme
  , symbol
  , dash
  , colon
  , dot
  , hash
  , char
  , sepBy
  , eol
  , eof
  , choice
  , integer
  , many
  , manyTill
  , charseq
  , lowerChar
  , Parsing.parse
  , Parser
  ) where
  

import Text.Megaparsec hiding(State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Text as T
import Data.Void

type Parser = Parsec Void T.Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

dash :: Parser T.Text
dash = symbol "-"

colon :: Parser T.Text
colon = symbol ":"

hash :: Parser Char
hash = char '#'

dot :: Parser Char
dot = char '.'

integer :: Parser Int
integer = lexeme L.decimal

charseq :: Parser T.Text
charseq = T.pack <$> manyTill L.charLiteral eol

parse :: Parser a -> FilePath -> IO (Either (ParseErrorBundle T.Text Void) a)
parse p f = runParser p f <$> T.pack <$> readFile f
