import System.Environment
import Data.List

-- this is how to get hold of command line arguments and the program name
main = do
   args <- getArgs
   progName <- getProgName
   putStrLn "The arguments are:"
   mapM putStrLn args
   putStrLn "The program name is:"
   putStrLn progName