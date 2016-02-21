-- pattern matching
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- recursion
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- pattern matching with tuples
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- pattern matching with lists and list comprehensions
x = [a+b | (a, b) <- [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]]

-- as-pattern
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- guards and where block. indentation matters to the compiler
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

-- pattern matching in where block
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- function in where block
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let expressions take the form of let <bindings> in <expression>
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

-- using let to define a function in a local scope
let1 = [let square x = x * x in (square 5, square 3, square 2)]
-- pattern matching with a let expression
let2 = (let (a, b, c) = (1, 2, 3) in a+b+c) * 100

-- let in a list comprehension
calcBmis2 :: [(Double, Double)] -> [Double]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
-- the names defined in the let are visible in the output and everything after the let
calcBmis3 :: [(Double, Double)] -> [Double]
calcBmis3 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

-- case expressions - functions head1 and head2 are equivalent:
head1 :: [a] -> a
head1 [] = error "No head for empty lists!"
head1 (x:_) = x

head2 :: [a] -> a
head2 xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

-- describeList1 and describeList2 are equivalent
describeList1 :: [a] -> String
describeList1 ls = "The list is " ++ case ls of [] -> "empty."
                                                [x] -> "a singleton list."
                                                xs -> "a longer list."

describeList2 :: [a] -> String
describeList2 ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
