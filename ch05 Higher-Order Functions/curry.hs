-- every Haskell function is curried - it always takes exactly one parameter

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- some examples of the usefullness of zipWith
addedLists = zipWith' (+) [4,2,5,6] [2,6,2,3]
maxBetween = zipWith' max [6,3,2,1] [7,3,1,5]
phrases    = zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
nestedZip  = zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y
-- using a lambda makes it clear that flip produces a new function
flip'' f = \y x -> f x y


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

elephants = takeWhile (/=' ') "elephants know how to party"

-- the sum of all odd squares that are less than 10,000
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- Collatz chain
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain(n*3+1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15
-- the same function using a lambda expression in place of isLong
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))


-- mapping a function that takes multiple parameters
listOfFuns = map (*) [0..]
-- take the element at index 4 and pass it the remaining parameter
result = (listOfFuns !! 4) 5