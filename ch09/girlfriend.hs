import System.IO
import Data.Char

main = do
    -- openFile has type signature openFile :: FilePath -> IOMode -> IO Handle
    handle <- openFile "girlfriend.txt" ReadMode
    -- this works much like getContents, except it reads from a known file handle
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- the above could have been written using withFile, which ensures the file handle is always closed:
{-
main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
-}

-- or could have been even more concisely using readFile, which also closes the file handle:
{-
main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
-}

-- this is how we could write a capitalised version out:
{-
main = do
    -- appendFile behaves like readFile except that it doesn't stomp down the file before writing
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)
-}
