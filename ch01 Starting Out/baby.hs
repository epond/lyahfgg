doubleMe x = x + x 

doubleUs x y = x * 2 + y * 2

doubleSmallNumber x = if x > 100
                      then x
                      else x*2

doubleSmallNumber' x = doubleSmallNumber x + 1

repl3 = replicate 3 10

-- Consider the set comprehension {2·x|x ∈ N, x ≤ 10}
-- This can be expressed as take 10 [2,4..] or as a list comprehesion like:
listComp1 = [x*2 | x <- [1..10]]
-- Then filtering can be done like:
listComp2 = [x*2 | x <- [1..10], x*2 >= 12]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
twoGens = [x+y | x <- [1,2,3], y <- [10,100,1000]]
twoGensPred = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

nouns = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]
phrases = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
nestedListComp = [ [ x | x <- xs, even x ] | xs <- xxs]

tuple1 = (3, 'a', "hello")

-- zip takes two lists, producing one list by joining the matching elements into pairs
zipFruit = zip [1..] ["apple", "orange", "cherry", "mango"]

-- Finding the Right Triangle
rightTriangles = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]