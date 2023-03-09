
import System.Environment
import Text.Parsec qualified as P
import Text.Parsec.String qualified as P
import Data.List (subsequences, intersperse, maximumBy)

import System.Random (randomRIO, RandomGen (split))
import Control.Monad (replicateM)



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


itemsPropBitmapDotProduct :: (Item -> Int) -> [Item] -> [Int] -> Int
itemsPropBitmapDotProduct prop items bitmap =  sum $ zipWith (*) (map prop items) bitmap

solutionValue :: [Int] -> Knapsack -> Int
solutionValue bitmap knapsack = if itemsPropBitmapDotProduct weight (items knapsack) bitmap > maxWeight knapsack  then 0 else itemsPropBitmapDotProduct cost (items knapsack) bitmap

randomSolution :: Int -> IO[Int]
randomSolution individualSize = replicateM individualSize $ randomRIO (0,1)

randomPopulation :: Int -> Int -> IO[[Int]]
randomPopulation populationSize individualSize = replicateM populationSize (randomSolution individualSize)

miniTournament :: Knapsack -> [[Int]] -> IO[Int]
miniTournament knapsack population = do
  randIndex1 <- randomRIO (0,populationSize-1)
  randIndex2 <- randomRIO (0,populationSize-1)
  let potentialParent1 = population!!randIndex1
  let potentialParent2 = population!!randIndex2
  if solutionValue potentialParent1  knapsack >  solutionValue potentialParent2 knapsack then return potentialParent1  else return potentialParent2
  where
  populationSize = length population


crossover :: [Int] -> [Int] -> IO[[Int]]
crossover parent1 parent2 =
  let middle = length parent1 `div` 2
  in
  return [take middle parent1 ++ drop middle parent2, take middle parent2 ++ drop middle parent1]

mutate :: [Int] ->IO[Int]
mutate parent = do
  bitIndex <- randomRIO (0, length parent-1)
  return (take bitIndex parent ++ [if parent!!bitIndex == 0 then 1 else 0] ++ drop (bitIndex+1) parent)

nextGenParents :: [[Int]] -> Knapsack -> Float -> Float -> IO[[Int]]
nextGenParents population knapsack crossoverRate mutationRate = do
  parent1 <- miniTournament knapsack population
  parent2 <- miniTournament knapsack population
  randCrossover <- randomRIO (0,1)::IO Float
  randMutationParent1 <- randomRIO (0,1)::IO Float
  randMutationParent2 <- randomRIO (0,1)::IO Float
  [crossedParent1, crossedParent2] <- if randCrossover <= crossoverRate then crossover parent1 parent2 else return [parent1, parent2]
  mutatedParent1 <- if randMutationParent1 <= mutationRate then mutate crossedParent1 else return crossedParent1
  mutatedParent2 <- if randMutationParent2 <= mutationRate then mutate crossedParent1 else return crossedParent1
  return [mutatedParent1, mutatedParent2]

selectParentTournament knapsack population=
  do
  parent1 <- miniTournament knapsack population
  parent2 <- miniTournament knapsack population
  return [parent1, parent2]

genFitnessMean :: [[Int]] -> Knapsack -> Int
genFitnessMean newGen knapsack = sum (map (`solutionValue` knapsack) newGen) `div` length newGen


getTwoFittest :: Knapsack -> [[Int]] -> ([Int], [Int])
getTwoFittest knapsack population = 
  let (max1, max2) = foldl (\(max1, max2) x -> if solutionValue x knapsack  > solutionValue max1 knapsack then (x, max1) else if solutionValue x knapsack > solutionValue max2 knapsack then (max2, x) else (max1, max2)) (head population, head population) population
  in (max1, max2)


nextGen :: Knapsack -> Int -> Float -> Float -> Float -> [[Int]] -> IO [[Int]]
nextGen knapsack populationSize crossoverRate mutationRate reproductionRate population = do
  reproductionRand <- randomRIO (0,1)::IO Float
  
  if reproductionRand <= reproductionRate then do
    let newPopSize = (populationSize `div` 2)-1
    let twoFittest = getTwoFittest knapsack population
    newGen <- replicateM newPopSize $ nextGenParents population knapsack crossoverRate mutationRate
    return (fst twoFittest : snd twoFittest : concat newGen )
  else
    do
    let newPopSize = populationSize `div` 2
    newGen <- replicateM newPopSize $ nextGenParents population knapsack crossoverRate mutationRate
    return $ concat newGen 


geneticIterations :: Knapsack -> Int -> Float -> Float -> Float -> [[Int]] -> Int -> IO [[Int]]
geneticIterations knapsack populationSize crossoverRate mutationRate reproductionRate population 0 = return population
geneticIterations knapsack populationSize crossoverRate mutationRate reproductionRate population n = do
  newPopulation <- nextGen knapsack populationSize crossoverRate mutationRate reproductionRate population
  geneticIterations knapsack populationSize crossoverRate mutationRate reproductionRate newPopulation (n-1)

bestGeneticSolution :: Knapsack ->[[Int]] -> Maybe [Int]
bestGeneticSolution knapsack solutions=
  let bestGenSolution = maximumBy (\x y -> if solutionValue x knapsack > solutionValue y knapsack then GT else LT) solutions
  in
  if solutionValue bestGenSolution knapsack >= minCost knapsack then Just bestGenSolution else Nothing

geneticAlg knapsack=
  do
  let populationSize = 150
  let crossoverRate = 0.5
  let mutationRate = 0.5
  let numIterations = 500
  let reproductionRate = 0.7
  initialPopulation <- randomPopulation populationSize (length $ items knapsack)
  finalPopulation <- geneticIterations knapsack populationSize crossoverRate mutationRate reproductionRate initialPopulation  numIterations
  let best = bestGeneticSolution knapsack finalPopulation
  print $ flip solutionValue knapsack $ maximumBy (\x y -> if solutionValue x knapsack > solutionValue y knapsack then GT else LT) finalPopulation
  case bestGeneticSolution knapsack finalPopulation of 
    Just solution -> printBitmap solution
    Nothing -> print False


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
knapsack ("-o" : input_file) = do
  parsed_knapsack <- parseInput input_file
  case parsed_knapsack of
    Nothing -> error "parsing knapsack input failed"
    Just knapsack -> geneticAlg knapsack
knapsack _ = error "invalid arg"

--read knapsack input from file or stdin
readInput :: [FilePath] -> IO String
readInput [] =  getContents
readInput [input_file] = readFile input_file

-- parse knapsack input
parseInput :: [FilePath] -> IO (Maybe Knapsack)
parseInput args = do
  input <- readInput args
  return (case P.runParser knapsackParser () input input of
          Left _  -> Nothing
          Right x -> Just x)

--map found solution to bitmap
solutionBitmap :: [Item] -> [Item] -> [Int]
solutionBitmap allItems solutionItems =  map (\item -> if item `elem` solutionItems then 1 else 0) allItems

--map bitmap int array to string format with spaces
printBitmap :: [Int] -> IO()
printBitmap arr = do
  putStr "Solution: "
  putStrLn $  map (\char -> (if (char == ',') || (char == '"') then ' ' else char)) $ show arr

-- find solution using bruteforce solution (try all combinations, O(2^n))
bruteforce :: Knapsack -> IO ()
bruteforce (Knapsack maxWeight minCost items) = case bestSolution (0, Nothing) maxWeight minCost (subsequences items) of
  Nothing -> print False
  Just solution -> printBitmap $ solutionBitmap items solution

-- get maybe best solution (nothing if solution does not exist)
bestSolution ::  (Int, Maybe [Item]) -> Int -> Int -> [[Item]] -> Maybe [Item]
bestSolution best _ _ [] = snd best
bestSolution (currentBestCost, solutionItems)  maxWeight minCost (x:xs) =
  let isValidAndBetterSolution = isValidAndBetter currentBestCost maxWeight minCost x
  in
  if fst isValidAndBetterSolution
    then bestSolution (snd isValidAndBetterSolution, Just x) maxWeight minCost xs
    else bestSolution (currentBestCost, solutionItems) maxWeight minCost xs

-- check if given solution is valid and is better than current best found
isValidAndBetter :: Int->Int -> Int -> [Item] -> (Bool, Int)
isValidAndBetter currentBest maxWeight minCost items =
  let (totalWeight, totalCost) = foldl (\ (totalWeight, totalCost) (Item weight cost) -> (totalWeight + weight, totalCost + cost)) (0, 0) items
  in
  (totalWeight <= maxWeight && totalCost >= minCost && totalCost>currentBest, totalCost)




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


main :: IO ()
main = do
    args <- getArgs
    knapsack args
