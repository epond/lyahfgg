import Control.Monad
import Data.Char

main = do
    -- getContents reads everying from standard input until it encounters end of file.
    -- it uses lazy i/o so it only reads when it needs to
    contents <- getContents
    putStrLn $ map toUpper contents