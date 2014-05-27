import Newtypes
import qualified Data.Map as Map
import qualified Data.List as List

-- the Eq typeclass is defined as follows:
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)

-- let's make a type of the Eq typeclass
data TrafficLight = Red | Yellow | Green
-- the instance keyword is for making our types instances of type classes
-- we need only implement == or /=, which is called the minimal complete definition
-- note how we fix the actual type from the type variable in the typeclass definition
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
-- we could have just derived Eq above for the same behaviour
-- but Show could do with more than just value constructors as strings:
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- subclassing in typeclasses is achieved through a class constraint in the class declaration, eg:
-- class (Eq a) => Num a where ...

-- parameterized types as instances of typeclasses. Maybe relates to Eq as follows
-- we want Maybe m to be part of Eq but m must be part of Eq first
-- instance (Eq m) => Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False

-- let's implement the JavaScript-like behaviour of interpreting values as booleans
class YesNo a where
    yesno :: a -> Bool
-- any number that isn't 0 is true in a boolean context
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
-- empty lists are false, all non-empty lists true
instance YesNo [a] where
    yesno [] = False
    yesno _  = True
-- bool implementation is trivial using the standard library identity function
instance YesNo Bool where
    yesno = id
-- we don't need a class constraint on a because we're not interested in its value
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing  = False
-- sure our trafic light can be yes or no
instance YesNo TrafficLight where
    yesno Red = False
    yesno _   = True
-- now to try it out...
isInt = yesno (3 :: Int)
isList = yesno "haha"
isMaybe = yesno $ Just 0
isLight = yesno Red
-- it remains to implement the if behaviour
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoValue yesResult noResult =
    if yesno yesnoValue
        then yesResult
        else noResult
-- and try it out:
yesnoList  = yesnoIf "haha" "Yeah" "No"
yesnoMaybe = yesnoIf (Just 0) "Yeah" "No"


-- the Functor type class is for things that can be mapped over. it looks like this (with prefix 'my'):
class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b
-- Functor wants a type constructor that takes one type, and not a concrete type

-- for lists, fmap and map are the same thing:
listMap  = map (*2) [1..3]
listFmap = fmap (*2) [1..3]

-- since Maybe is also box-like, it is part of Functor like this:
-- instance Functor Maybe where
--     fmap f (Just x) = Just (f x)
--     fmap f Nothing = Nothing

-- our Tree can be a Functor too, which is implemented here using recursion
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

mappedTree = fmap (*4) (foldr treeInsert EmptyTree [5,7,3])

-- Either is part of Functor type class like this:
-- instance Functor (Either a) where
--     fmap f (Right x) = Right (f x)
--     fmap f (Left x) = Left x
-- in which the fmap type signature would look like (b -> c) -> Either a b -> Either a c

-- the exercise at the end of chapter 7 asks how Map k is made an instance of Functor...
instance (Ord k) => MyFunctor (Map.Map k) where
    myfmap f mp = Map.fromList $ List.map (\(x,y) -> (x,f y)) $ Map.toList mp

mappedMap = myfmap (*2) $ Map.fromList [('a',1),('b',2)]