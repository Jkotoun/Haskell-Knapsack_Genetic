import BruteforceKnapsack(bruteforce)
import GeneticKnapsack (geneticAlg)

import Types
import System.Environment
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import Control.Applicative ((<$>)) --needed for version 7

-- map bitmap int array to string format with spaces
printSolutionBitmap :: [Int] -> IO ()
printSolutionBitmap arr = do
  putStr "Solution "
  putStrLn $ map (\character -> if (character == ',') || (character == '"') then ' ' else character) $ show arr

-------input processing--------

runGeneticAlgo :: Knapsack -> IO ()
runGeneticAlgo knapsack = do
  result <- geneticAlg knapsack populationSize crossoverRate mutationRate reproductionRate numIterations
  case result of
    Left _ -> print False
    Right solution -> printSolutionBitmap solution
  where
    populationSize = 150
    crossoverRate = 0.5
    mutationRate = 0.5
    reproductionRate = 0.3
    numIterations = 500

runBruteforce :: Knapsack -> IO ()
runBruteforce knapsack = do
  result <- bruteforce knapsack
  case result of
    Left _ -> print False
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
  _ <- P.spaces
  _ <- P.string "maxWeight:"
  _ <- P.spaces 
  maximalItemsWeight <- read <$> P.many1 P.digit
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.spaces
  _ <- P.many1 (P.string "minCost:")
  _ <- P.spaces
  minimalItemsCost <- read <$> P.many1 P.digit
  _ <- P.spaces
  _ <- P.string "items:"
  _ <- P.manyTill P.anyChar P.newline
  itemsList <- P.many1 itemParser
  return (Knapsack {maxWeight = maximalItemsWeight, minCost = minimalItemsCost, items = itemsList})

-- parser for one item from list of items
itemParser :: P.Parser Item
itemParser = do
  _ <- P.spaces
  _ <- P.string "Item"
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.spaces
  _ <- P.string "weight:"
  _ <- P.spaces
  itemWeight <- read <$> P.many1 P.digit
  _ <- P.spaces
  _ <- P.string "cost:"
  _ <- P.spaces
  itemCost <- read <$> P.many1 P.digit
  _ <- P.manyTill P.anyChar P.newline
  _ <- P.manyTill P.anyChar P.newline



  return (Item {weight = itemWeight, cost = itemCost})

main :: IO ()
main = do
  args <- getArgs
  solveKnapsack args
