import BruteforceKnapsack(bruteforce)
import Data.Functor.Identity
import GeneticKnapsack (geneticAlg)

import Types
import System.Environment
import System.Random (randomRIO)
import Text.Parsec qualified as P
import Text.Parsec.String qualified as P

----general helper functions--------
-- map bitmap int array to string format with spaces
printSolutionBitmap :: [Int] -> IO ()
printSolutionBitmap arr = do
  putStr "Solution "
  putStrLn $ map (\char -> if (char == ',') || (char == '"') then ' ' else char) $ show arr

-------input processing--------

runGeneticAlgo knapsack = do
  result <- geneticAlg knapsack populationSize crossoverRate mutationRate reproductionRate numIterations
  case result of
    Left failure -> print False
    Right solution -> printSolutionBitmap solution
  where
    populationSize = 150
    crossoverRate = 0.5
    mutationRate = 0.5
    reproductionRate = 0.3
    numIterations = 500

runBruteforce knapsack = do
  result <- bruteforce knapsack
  case result of 
    Left failure -> print failure
    Right solution -> printSolutionBitmap solution

-- call functions by command line args
solveKnapsack :: [String] -> IO ()
solveKnapsack ("-i" : input_file) = do
  parsed_knapsack <- parseInput input_file
  case parsed_knapsack of
    Nothing -> error "parsing knapsack input failed"
    Just knapsack -> print knapsack
solveKnapsack ("-b" : input_file) = do
  parsed_knapsack <- parseInput input_file
  case parsed_knapsack of
    Nothing -> error "parsing knapsack input failed"
    Just knapsack -> runBruteforce knapsack
solveKnapsack ("-o" : input_file) = do
  parsed_knapsack <- parseInput input_file
  case parsed_knapsack of
    Nothing -> error "parsing knapsack input failed"
    Just knapsack -> runGeneticAlgo knapsack
solveKnapsack _ = error "invalid arg"

-- read knapsack input from file or stdin
readInput :: [FilePath] -> IO String
readInput [] = getContents
readInput [input_file] = readFile input_file
readInput _ = error "invalid args"

-- parse knapsack input
parseInput :: [FilePath] -> IO (Maybe Knapsack)
parseInput args = do
  input <- readInput args
  return
    ( case P.runParser knapsackParser () input input of
        Left _ -> Nothing
        Right x -> Just x
    )

-- input parser
knapsackParser :: P.Parser Knapsack
knapsackParser = do
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many (P.string " ")
  _ <- P.many1 (P.string "maxWeight:")
  _ <- P.many (P.string " ")
  maxWeight <- read <$> P.many1 P.digit
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many (P.string " ")
  _ <- P.many1 (P.string "minCost:")
  _ <- P.many (P.string " ")
  minCost <- read <$> P.many1 P.digit
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.many (P.string " ")
  _ <- P.many1 (P.string "items:")
  _ <- P.manyTill P.anyChar P.newline
  items <- P.many itemParser
  return (Knapsack {maxWeight = maxWeight, minCost = minCost, items = items})

-- parser for one item from list of items
itemParser :: P.ParsecT String u Data.Functor.Identity.Identity Item
itemParser = do
  _ <- P.many (P.string " ")
  _ <- P.many1 (P.string "Item")
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
  return (Item {weight = weight, cost = cost})

main :: IO ()
main = do
  args <- getArgs
  solveKnapsack args