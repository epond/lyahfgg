-- a left fold takes values from the head (left) of the list, and the binary function's left parameter is the accumulator
suml xs = foldl (\acc x -> acc + x) 0 xs
-- a right fold takes values from the end (right) of the list, and the binary function's right parameter is the accumulator
sumr xs = foldr (\x acc -> x + acc) 0 xs
-- if you have a function like foo a = bar b a you can write it as foo = bar b because of currying
sum1 = foldl (+) 0

-- right folds are usually suitable for building up new lists from a list
mapf :: (a -> b) -> [a] -> [b]
mapf f xs = foldr (\x acc -> f x : acc) [] xs

-- left fold is suitable for reversing a list
reversef :: [a] -> [a]
reversef xs = foldl (\acc x -> x : acc) [] xs
-- since the above flips the cons parameters, and with curry simplification it can be rewritten as:
reversef' = foldl (flip (:)) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- foldl1 and foldr1 assume the accumulator is the first encountered element of the list
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

-- product starts with 1 and multiplies each element with the accumulator
product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

-- to get the last element we always set the current element as the accumulator
last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- foldr will work on infinite lists when the binary function doesn’t always need to evaluate its second parameter
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- scanl and scanr are like foldl and foldr but a list of all accumulator states is returned
additions = scanl (+) 0 [3,5,2,1]
-- additions = [0,3,8,10,11]

-- How many elements does it take for the sum of the square roots of all natural numbers to exceed 1,000?
-- We use takeWhile instead of filter due to the infinite list of increasing numbers
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1