import System.Environment
import Text.Parsec qualified as P
import Text.Parsec.String qualified as P



data Item = Item
  { weight :: Int,
    cost :: Int
  }
  deriving (Show)

data Knapsack = Knapsack
  { maxWeight :: Int,
    minCost :: Int,
    items :: [Item]
  }
  deriving (Show)


-- call functions by command line args
knapsack :: [String] -> IO ()
knapsack ("-i" : input_file) = parseInput input_file
-- knapsack ("-b" : input_file) = bruteforce input_file
-- knapsack ("-o" : input_file) = evolutionAlg input_file
knapsack _ = error "invalid arg"

parseFromFile p fname = do
  input <- readFile fname
  return (P.runParser p () fname input)

-- parse input file or stdin
-- parseInput :: [FilePath] -> IO ()
parseInput [] = interact $ \s -> case P.runParser knapsackParser () "" s of
        Left perror  -> error "parser error" perror
        Right x -> show x
parseInput [input_file] = do
    input <- readFile input_file
    case  P.runParser knapsackParser () input_file input of
        Left perror -> error "parser error" perror
        Right x -> print x

-- bruteforce method
bruteforce :: Knapsack -> IO()
bruteforce knapsack = putStrLn "bruteforce stdin"

-- evolution algorithm method
evolutionAlg :: Knapsack -> IO()
evolutionAlg knapsack = putStrLn "evolutionAlg stdin"

main = do
    args <- getArgs
    knapsack args


-- input file parser
knapsackParser :: P.Parser Knapsack
knapsackParser = do
  P.manyTill P.anyChar P.newline
  P.many1 (P.string "maxWeight:")
  P.many (P.string " ")
  maxWeight <- read <$> P.many1 P.digit
  P.manyTill P.anyChar P.newline
  P.many1 (P.string "minCost:")
  P.many (P.string " ")
  minCost <- read <$> P.many1 P.digit
  P.manyTill P.anyChar P.newline
  P.many1 (P.string "items:")
  P.manyTill P.anyChar (P.string "[")
  P.manyTill P.anyChar P.newline
  items <- P.many item
  return (Knapsack {maxWeight = maxWeight, minCost = minCost, items = items})

-- parser for one item from list of items
item = do
  P.many (P.string " ")
  P.many1 (P.string "Item")
  P.many (P.string " ")
  P.manyTill P.anyChar (P.string "{")
  P.manyTill P.anyChar P.newline
  P.many (P.string " ")
  P.many1 (P.string "weight:")
  P.many (P.string " ")
  weight <- read <$> P.many1 P.digit
  P.manyTill P.anyChar P.newline
  P.many (P.string " ")
  P.many1 (P.string "cost:")
  P.many (P.string " ")
  cost <- read <$> P.many1 P.digit
  P.manyTill P.anyChar P.newline
  P.manyTill P.anyChar P.newline

  return (Item{weight=weight, cost=cost})