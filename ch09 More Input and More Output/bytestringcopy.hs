import System.Environment
import System.Directory
import System.IO
import Control.Exception
import qualified Data.ByteString.Lazy as B

-- this program takes two filenames as command-line arguments and copies the first file into the second file.
-- it duplicates the behaviour of the copyFile function in System.Directory
main = do
    (fileName1:fileName2:_) <- getArgs
    copy fileName1 fileName2
copy source dest = do
    contents <- B.readFile source
    bracketOnError
        (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            B.hPutStr tempHandle contents
            hClose tempHandle
            renameFile tempName dest)