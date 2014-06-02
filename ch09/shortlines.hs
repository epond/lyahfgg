main = do
    -- the i/o part is as short as possible
    contents <- getContents
    putStr (shortLinesOnly contents)

-- the above is a common pattern which is the job of the 'interact' function, so we could have done:
-- main = interact shortLinesOnly

-- the meat of the program is now just a pure function
shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines