module Day3 where
import Parsing

file :: FilePath
file = "./src/day3.txt"

data Square = Empty
            | Tree
            deriving(Eq, Show)
data Slope = Slope Int Int Int Int

dotParser :: Parser Square
dotParser = Empty <$ dot

hashParser :: Parser Square
hashParser = Tree <$ hash

entry :: Parser [Square]
entry = manyTill (choice [dotParser, hashParser]) eol

parser :: Parser [[Square]]
parser = many entry

countTrees :: Slope -> [[Square]] -> Int -> Int
countTrees _ [] acc = acc
countTrees (Slope x y movX movY) (row:rows) acc
  | current == Tree = countTrees (Slope (x + movX) y movX movY) nextRows (acc + 1)
  | otherwise = countTrees (Slope (x + movX) y movX movY) nextRows acc
  where
    nextRows = case movY of
                 1 -> rows
                 _ -> (drop (movY - 1) rows) 
    -- cycle is needed here because the input repeats;
    -- we can safely request `x` from the cycled row
    current = (cycle row) !! x

main :: IO ()
main = do
  result <- parse parser file
  case result of
    Right entries ->
      let first = countTrees  (Slope 0 0 1 1) entries 0
          second = countTrees (Slope 0 0 3 1) entries 0
          third = countTrees  (Slope 0 0 5 1) entries 0
          fourth = countTrees (Slope 0 0 7 1) entries 0
          fifth = countTrees  (Slope 0 0 1 2) entries 0
       in
        print $ first  * second * third * fourth * fifth

    Left e -> print e
