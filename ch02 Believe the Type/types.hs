-- If you don't know what the type declaration should be, you can omit it and use :t
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

readInt = read "5" - 2
readTuple = read "(3, 'a')" :: (Int, Char)
enumOrdering = [LT .. GT]
upperBound = maxBound :: Int

-- fromIntegral has signature: fromIntegral :: (Num b, Integral a) => a -> b
-- fromIntegral takes an integral number and turns it into a more general number
intToFloat = fromIntegral (length [1,2,3,4]) + 3.2