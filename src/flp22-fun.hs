
import System.Environment
import Data.List  

parseArgs :: [String] -> IO()
parseArgs ("-i":input_file) = showInstance input_file
parseArgs ("-b":input_file) = bruteforce input_file
parseArgs ("-o":input_file) = evolutionAlg input_file
parseArgs _ = error "invalid arg"

showInstance [] = putStrLn "show empty"
showInstance [file] = putStrLn "show file"

bruteforce [] = putStrLn "bruteforce stdin"
bruteforce [file] = putStrLn "bruteforce file"

evolutionAlg [] = putStrLn "evolutionAlg stdin"
evolutionAlg [file] = putStrLn "evolutionAlg file"


main = do
    args <- getArgs
    parseArgs args