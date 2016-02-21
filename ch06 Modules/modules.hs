-- a module defines functions, types and typeclasses. a program is a collection of modules.
-- useful search engine for Haskell: http://www.haskell.org/hoogle

-- standard way of importing a module
import Data.List
import Data.Char
-- this way can get around naming clashes
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- . is used to reference functions from modules that have been imported and qualified.
-- it is interpreted this way if it lies between a qualified module name and a function, without whitespace
filtered = Map.filter (> "a") (Map.fromList [(5,"a"), (3,"b")])

-- an example of how standard library functions can count occurrences of words from text.
wordNums :: String -> [(String,Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

-- another example: is the first list contained in the second list? (same as Data.List.isInfixOf)
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

-- ceaser cipher
encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- greedy fold may run out of memory because it defers computation until the expression is complete
greedyFold = foldl (+) 0 (replicate 10000000 1)
-- strict fold (from Data.List) does not defer computation, and can get around problems like the above
strictFold = foldl' (+) 0 (replicate 10000000 1)

-- what’s the first natural number such that the sum of its digits equals 40?
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

-- optional values are expressed with Maybe. here we look up a value from an association list by its key:
findKey1 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey1 key [] = Nothing
findKey1 key ((k,v):xs)
    | key == k  = Just v
    | otherwise = findKey1 key xs

-- however this is better done as a fold. (same as Data.List.lookup)
findKey2 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey2 key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

-- Map.fromList creates a map from a list of pairs, discarding duplicates
phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

newBook = Map.insert "grace" "341-9021" phoneBook

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

intBook = Map.map string2digits phoneBook

-- so how can we handle duplicates?
phoneBook2 =
    [("betty", "555-2938")
    ,("betty", "342-2492")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("patsy", "943-2929")
    ,("patsy", "827-9162")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ,("penny", "555-2111")
    ]

-- Map.fromListWith will use the supplied function to resolve a conflict when it encounters a key that already exists
phoneBookToMap1 :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap1 xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2

-- or if the original values are singleton lists:
phoneBookToMap2 :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap2 xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs