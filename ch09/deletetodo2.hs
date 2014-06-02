import System.IO
import System.Directory
import Data.List
import Control.Exception

-- this version uses bracketOnError, which performs cleanup if an exception is raised,
-- to close and remove the temp file if an error happened.
main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                    [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        -- this is what to do on error
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        -- this is what to do if things are going well
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")
