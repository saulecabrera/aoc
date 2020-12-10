{-# LANGUAGE OverloadedStrings #-}

module Parsing
  (
   spaceConsumer
  , symbol
  , dash
  , byr
  , iyr
  , eyr
  , hgt
  , hcl
  , ecl
  , pid
  , cid
  , colon
  , cm
  , dot
  , hash
  , char
  , spaceChar
  , sepBy
  , eol
  , crlf
  , eof
  , choice
  , integer
  , Text.Megaparsec.many
  , Parsing.optional
  , manyTill
  , newline
  , notFollowedBy
  , charseq
  , lookAhead
  , try
  , L.decimal
  , L.charLiteral
  , lowerChar
  , Parsing.parse
  , alphaNumChar
  , Parser
  ) where
  

import Text.Megaparsec hiding(State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Text as T
import Data.Void
import Control.Applicative

type Parser = Parsec Void T.Text

optional :: Alternative f => f a -> f (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

dash :: Parser T.Text
dash = symbol "-"

colon :: Parser T.Text
colon = string ":"

byr :: Parser T.Text
byr = string "byr"

iyr :: Parser T.Text
iyr = string "iyr"

eyr :: Parser T.Text
eyr = string "eyr"

hgt :: Parser T.Text
hgt = string "hgt"

hcl :: Parser T.Text
hcl = string "hcl"

ecl :: Parser T.Text
ecl = string "ecl"

cm :: Parser T.Text
cm = string "cm"

pid :: Parser T.Text
pid = string "pid"

cid :: Parser T.Text
cid = string "cid"

hash :: Parser Char
hash = char '#'

dot :: Parser Char
dot = char '.'

integer :: Parser Int
integer = lexeme L.decimal

charseq :: Parser T.Text
charseq = T.pack <$> manyTill L.charLiteral spaceChar

parse :: Parser a -> FilePath -> IO (Either (ParseErrorBundle T.Text Void) a)
parse p f = runParser p f <$> T.pack <$> readFile f
