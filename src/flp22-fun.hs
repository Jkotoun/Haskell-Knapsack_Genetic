
import System.Environment
import Text.Parsec qualified as P
import Text.Parsec.String qualified as P
import Data.List (subsequences, intersperse)


data Item = Item
  { weight :: Int,
    cost :: Int
  }
  deriving (Show, Eq)

data Knapsack = Knapsack
  { maxWeight :: Int,
    minCost :: Int,
    items :: [Item]
  }
  deriving (Show,Eq)


-- call functions by command line args
knapsack :: [String] -> IO ()
knapsack ("-i" : input_file) = do
  parsed_knapsack <- parseInput input_file
  case parsed_knapsack of
    Nothing -> error "parsing knapsack input failed"
    Just knapsack -> print knapsack

knapsack ("-b" : input_file) = do
  parsed_knapsack <- parseInput input_file
  case parsed_knapsack of
    Nothing -> error "parsing knapsack input failed"
    Just knapsack -> bruteforce knapsack

knapsack _ = error "invalid arg"
solutionBitmap :: [Item] -> [Item] -> [Int]
solutionBitmap allItems solutionItems =  map (\item -> if item `elem` solutionItems then 1 else 0) allItems

printBitmap :: [Int] -> IO()
printBitmap arr = print $ map (\char -> (if (char == ',') || (char == '"') then ' ' else char)) $ show arr

readInput :: [FilePath] -> IO String
readInput [] =  getContents
readInput [input_file] = readFile input_file

-- file arg can be empty
parseInput :: [FilePath] -> IO (Maybe Knapsack)
parseInput args = do
  input <- readInput args
  return (case P.runParser knapsackParser () input input of
          Left _  -> Nothing
          Right x -> Just x)


-- bruteforce method
-- bruteforce :: Knapsack -> IO()
-- bruteforce :: Knapsack -> IO()

bruteforce (Knapsack maxWeight minCost items) = case bestSolution (0, Nothing) maxWeight minCost (subsequences items) of
  Nothing -> print False
  Just solution -> printBitmap $ solutionBitmap items solution

-- firstSolution :: Int -> Int -> [[Item]] -> Maybe [Item]
-- firstSolution _ _ [] = Nothing
-- firstSolution maxWeight minCost (x:xs) = if isValidSolution maxWeight minCost x then Just x else firstSolution maxWeight minCost xs

bestSolution ::  (Int, Maybe [Item]) -> Int -> Int -> [[Item]] -> Maybe [Item]
bestSolution best _ _ [] = snd best
bestSolution (currentBestCost, solutionItems)  maxWeight minCost (x:xs) = 
  let isValidAndBetterSolution = isValidAndBetter currentBestCost maxWeight minCost x
  in
  if fst isValidAndBetterSolution   
    then bestSolution (snd isValidAndBetterSolution, Just x) maxWeight minCost xs 
    else bestSolution (currentBestCost, solutionItems) maxWeight minCost xs



isValidAndBetter :: Int->Int -> Int -> [Item] -> (Bool, Int)
isValidAndBetter currentBest maxWeight minCost items =
  let (totalWeight, totalCost) = foldl (\ (totalWeight, totalCost) (Item weight cost) -> (totalWeight + weight, totalCost + cost)) (0, 0) items
  in
  (totalWeight <= maxWeight && totalCost >= minCost && totalCost>currentBest, totalCost)


-- evolution algorithm method
evolutionAlg :: Knapsack -> IO()
evolutionAlg (Knapsack maxWeight minCost items) = putStrLn "evolutionAlg stdin"




main :: IO ()
main = do
    args <- getArgs
    knapsack args


-- input file parser
knapsackParser :: P.Parser Knapsack
knapsackParser = do
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many1 (P.string "maxWeight:")
  _ <- P.many (P.string " ")
  maxWeight <- read <$> P.many1 P.digit
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many1 (P.string "minCost:")
  _ <- P.many (P.string " ")
  minCost <- read <$> P.many1 P.digit
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many1 (P.string "items:")
  _ <- P.manyTill P.anyChar (P.string "[")
  _ <- P.manyTill P.anyChar P.newline
  items <- P.many item
  return (Knapsack {maxWeight = maxWeight, minCost = minCost, items = items})

-- parser for one item from list of items
item = do
  _ <- P.many (P.string " ")
  _ <- P.many1 (P.string "Item")
  _ <- P.many (P.string " ")
  _ <- P.manyTill P.anyChar (P.string "{")
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many (P.string " ")
  _ <- P.many1 (P.string "weight:")
  _ <- P.many (P.string " ")
  weight <- read <$> P.many1 P.digit
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many (P.string " ")
  _ <- P.many1 (P.string "cost:")
  _ <- P.many (P.string " ")
  cost <- read <$> P.many1 P.digit
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.manyTill P.anyChar P.newline
  return (Item{weight=weight, cost=cost})