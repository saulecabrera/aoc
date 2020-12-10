module Day4 where
import Parsing
import qualified Data.Text as T

data YearTy = Birth
            | Issue
            | Expiration
            deriving (Eq, Show)

data Value = Year YearTy Int
           | I
           | Color
           | Height Int T.Text
           | Hair T.Text
           | Eye T.Text
           | PassportId T.Text
           | CountryId Int
            deriving (Eq, Show)

height = do
  hgt
  colon
  val <- decimal
  unit <- (many alphaNumChar)
  return $ Height val $ T.pack unit

hashed :: Parser T.Text
hashed = do
  delim <- (optional . try) $ char '#'
  cs <- (many alphaNumChar)
  case delim of
    Just d -> return $ T.pack $ d:cs
    Nothing -> return $ T.pack cs

expirationYear = (eyr *> colon *> decimal) >>= (\x -> pure $ Year Expiration x)
issueYear = (iyr *> colon *> decimal) >>= (\x -> pure $ Year Issue x)
birthYear = (byr *> colon *> decimal) >>= (\x -> pure $ Year Birth x)
eyeColor = (ecl *> colon *> hashed) >>= (\x -> pure $ Eye x) 
hairColor = (hcl *> colon *> hashed) >>= (\x -> pure $ Hair x) 
pId = (pid *> colon *> hashed) >>= (\x -> pure $ PassportId $ x) 
cId = (cid *> colon *> decimal) >>= (\x -> pure $ CountryId $ x) 

value :: Parser Value
value = choice [birthYear
               , issueYear
               , expirationYear
               , height
               , hairColor
               , eyeColor
               , pId
               , cId
               ]

entry :: Parser Value
entry = value <* spaceChar

file :: FilePath
file = "./src/day4.txt"

countValidPassports :: [[Value]] -> Int
countValidPassports ps =
  length $ filtered
    where
      filtered = filter (\p -> length p == 8 || optionalCountryId p) ps

optionalCountryId :: [Value] -> Bool
optionalCountryId vs = length filtered == 7
  where
    filtered = (filter (\v -> not $ isCountryId v) vs)

isCountryId :: Value -> Bool
isCountryId (CountryId _) = True
isCountryId _ = False

parser :: Parser [[Value]]
parser = sepBy (many entry) newline
  
-- Part 1 only
main :: IO ()
main = do
  results <- parse parser file
  case results of
    Right values -> print $ countValidPassports values
    Left e -> print e


