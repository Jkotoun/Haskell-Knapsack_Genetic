
module GeneticKnapsack (geneticAlg) where
import Types
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (maximumBy)
-- dot product of certain item prop and bitmap
itemsPropBitmapDotProduct :: (Item -> Int) -> [Item] -> [Int] -> Int
itemsPropBitmapDotProduct prop itemsList bitmap = sum $ zipWith (*) (map prop itemsList) bitmap

-- value of solution as binary array for given knapsack problem input
solutionFitness :: [Int] -> Knapsack -> Int
solutionFitness bitmap knapsack = if itemsPropBitmapDotProduct weight (items knapsack) bitmap > maxWeight knapsack then 0 else itemsPropBitmapDotProduct cost (items knapsack) bitmap

-- random solution as binary array for given
randomSolution :: Int -> IO [Int]
randomSolution individualSize = replicateM individualSize $ randomRIO (0, 1)

-- create population of random solutions
randomPopulation :: Int -> Int -> IO [[Int]]
randomPopulation populationSize individualSize = replicateM populationSize (randomSolution individualSize)

-- select random 2 individuals from population and return better of them
miniTournament :: Knapsack -> [[Int]] -> IO [Int]
miniTournament knapsack population = do
  randIndex1 <- randomRIO (0, populationSize - 1)
  randIndex2 <- randomRIO (0, populationSize - 1)
  let potentialParent1 = population !! randIndex1
  let potentialParent2 = population !! randIndex2
  if solutionFitness potentialParent1 knapsack > solutionFitness potentialParent2 knapsack then return potentialParent1 else return potentialParent2
  where
    populationSize = length population

-- crossover 2 individuals in middle
crossover :: [Int] -> [Int] -> IO [[Int]]
crossover parent1 parent2 =
  let middle = length parent1 `div` 2
   in return [take middle parent1 ++ drop middle parent2, take middle parent2 ++ drop middle parent1]

-- mutate random bit
mutate :: [Int] -> IO [Int]
mutate parent = do
  bitIndex <- randomRIO (0, length parent - 1)
  return (take bitIndex parent ++ [if parent !! bitIndex == 0 then 1 else 0] ++ drop (bitIndex + 1) parent)

-- select two parents from population by minitournament, perform crossover and mutation with respect to crossover and mutation rate
nextGenParents :: [[Int]] -> Knapsack -> Float -> Float -> Float -> IO [[Int]]
nextGenParents population knapsack crossoverRate mutationRate reproductionRate = do
  parent1 <- miniTournament knapsack population
  parent2 <- miniTournament knapsack population
  reproductionRand <- randomRIO (0, 1) :: IO Float
  if reproductionRand <= reproductionRate then
    return [parent1, parent2]
  else do
    randCrossover <- randomRIO (0, 1) :: IO Float
    randMutationParent1 <- randomRIO (0, 1) :: IO Float
    randMutationParent2 <- randomRIO (0, 1) :: IO Float
    [crossedParent1, crossedParent2] <- if randCrossover <= crossoverRate then crossover parent1 parent2 else return [parent1, parent2]
    mutatedCrossedParent1 <- if randMutationParent1 <= mutationRate then mutate crossedParent1 else return crossedParent1
    mutatedCrossedParent2 <- if randMutationParent2 <= mutationRate then mutate crossedParent2 else return crossedParent2
    return [mutatedCrossedParent1, mutatedCrossedParent2]

-- create new generation of population, two variants with respect to reproductionRate
nextGen :: Knapsack -> Int -> Float -> Float -> Float -> [[Int]] -> IO [[Int]]
nextGen knapsack populationSize crossoverRate mutationRate reproductionRate population = do
  let halfPopulation = populationSize `div` 2 -- each nextGenParent function execution returns 2 new individuals
  newGen <- replicateM halfPopulation $ nextGenParents population knapsack crossoverRate mutationRate reproductionRate
  return $ concat newGen

-- generate N new generations
geneticIterations :: Knapsack -> Int -> Float -> Float -> Float -> [[Int]] -> Int -> IO [[Int]]
geneticIterations _ _ _ _ _ population 0 = return population
geneticIterations knapsack populationSize crossoverRate mutationRate reproductionRate population n = do
  newPopulation <- nextGen knapsack populationSize crossoverRate mutationRate reproductionRate population
  geneticIterations knapsack populationSize crossoverRate mutationRate reproductionRate newPopulation (n - 1)

-- select best solution from final population
bestGeneticSolution :: Knapsack -> [[Int]] -> Maybe [Int]
bestGeneticSolution knapsack solutions =
  let bestGenSolution = maximumBy (\x y -> if solutionFitness x knapsack > solutionFitness y knapsack then GT else LT) solutions
   in if solutionFitness bestGenSolution knapsack >= minCost knapsack then Just bestGenSolution else Nothing

-- run genetic algo with parameters and return resulting IO
geneticAlg :: Knapsack -> Int -> Float -> Float-> Float -> Int -> IO (Either Bool [Int])
geneticAlg knapsack populationSize crossoverRate mutationRate reproductionRate numIterations  =
  do

    initialPopulation <- randomPopulation populationSize (length $ items knapsack)
    finalPopulation <- geneticIterations knapsack populationSize crossoverRate mutationRate reproductionRate initialPopulation numIterations
    case bestGeneticSolution knapsack finalPopulation of
        Nothing -> return $ Left False
        Just solution -> return $ Right solution
