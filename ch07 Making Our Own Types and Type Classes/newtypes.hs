module Newtypes where

import qualified Data.Map as Map

-- the data keyword can be used to define a new type
-- value constructors are functions that return a value of a data type
-- the part before the = is the type constructor, with value constructors after it separated by |
-- multiple value constructors can be used to represent a sum algebraic data type
data SimpleShape = SimpleCircle Float Float Float | SimpleRectangle Float Float Float Float
    deriving (Show)

-- a function that takes a shape and returns its area
simpleArea :: SimpleShape -> Float
simpleArea (SimpleCircle _ _ r) = pi * r ^ 2
simpleArea (SimpleRectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

circArea1 = simpleArea $ SimpleCircle 10 20 3

-- value constructors can do all the things functions do, eg partial application:
increasingCircles = map (SimpleCircle 10 20) [4,5,6,6]

-- data type and value constructor can have the same name
data Point = Point Float Float deriving (Show)
-- now our shapes are more understandable
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

circArea2 = area $ Circle (Point 10 20) 3

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
    = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

nudgedCircle = nudge (Circle (Point 34 34) 10) 5 10

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- Now we can make shapes that are located at the origin of the coordinate system
-- and then nudge them to where we want them to be
rect = nudge (baseRect 40 100) 60 23


-- record syntax provides named fields, accessor functions and better show output
-- this can be used to represent a product algebraic data type
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavour :: String } deriving (Show)

-- we can still instantiate in the normal way:
bud = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- but record syntax lets us name fields:
larry = Person {firstName="Larry", lastName="Smith", age=38, height=191.8, phoneNumber="08766332123", flavour="Pistachio"}
-- and we have auto-generated accessor functions:
larryHeight = height larry

-- 'Maybe a' is an example of a type constructor. 'Maybe Char' is an example of a concrete type.

-- lets define a Vector whose components are a Num, but we don't add the type constraint in the data declaration.
data Vector a = Vector a a a deriving (Show)
-- best practice is to use type constraints in the functions where they make sense, so here we constrain a to a Num
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n
vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

-- to be able to derive Eq all the fields must themselves be part of the Eq typeclass
data Dude = Dude { dudeFirstName :: String
                 , dudeLastName :: String
                 , dudeAge :: Int
                 } deriving (Eq, Show, Read)
mikeD = Dude {dudeFirstName = "Michael", dudeLastName = "Diamond", dudeAge = 43}
adRock = Dude {dudeFirstName = "Adam", dudeLastName = "Horovitz", dudeAge = 41}
mca = Dude {dudeFirstName = "Adam", dudeLastName = "Yauch", dudeAge = 44}
beastieBoys = [mca, adRock, mikeD]
idMikeDInBeasieBoys = mikeD `elem` beastieBoys

mysteryDude = "Dude {dudeFirstName = \"Michael\", dudeLastName = \"Diamond\", dudeAge = 43}"
-- when reading the string we need to tell Haskell why type to expect, in this case Dude
mysteryDudeValue = read mysteryDude :: Dude
-- but if Haskell can infer the type we don't need the type annotation
isMysteryDudeMikeD = read mysteryDude == mikeD

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
isSatAfterFri = Saturday > Friday
firstDayOfWeek = minBound :: Day
dayAfterTuesday = succ Tuesday
thursToSun = [Thursday .. Sunday]
allDays = [minBound .. maxBound] :: [Day]

-- type synonyms are like scala type aliases
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
-- now our function definitions become more meaningful, much better than a sea of Strings
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook
-- type synonyms can be parameterised
type AssocList k v = [(k, v)]
-- a function that gets a value by key can now have type of (Eq k) => k -> AssocList k v -> Maybe v

-- Either type - let's model a school locker supervisor
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
-- a function that searches for the code in a locker map. information about failure is included in the result type
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
-- here's an example locker map
lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken, "ZD39I"))
    ,(101,(Free, "JAH3I"))
    ,(103,(Free, "IQSA9"))
    ,(105,(Free, "QOTSA"))
    ,(109,(Taken, "893JJ"))
    ,(110,(Taken, "99292"))
    ]

-- using algebraic data types to implement our own list. Cons is a synonym for :
data MyList a = MyEmpty | Cons a (MyList a) deriving (Show, Read, Eq, Ord)

-- a fixity states how tightly the operator binds and whether it’s left or right-associative
-- infix constructors must begin with a colon. let's implement a list again
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
-- now let's create a function that adds two of our lists together
-- pattern matching works only on constructors
infixr 5  ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

-- let's implement a binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- singleton creates a tree with a single node
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree
-- treeInsert inserts an element into a tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)
-- treeElem checks if a node is in a tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
-- we can use a fold to build a tree from a list
myTree = foldr treeInsert EmptyTree [4,1,2,8,3]