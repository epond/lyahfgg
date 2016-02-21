import Control.Applicative

{-
A functor can be thought of as a box, or better yet as a value with a context.
the Functor type class looks like this:
class Functor f where              -- f is a type constructor that takes one type parameter
    fmap :: (a -> b) -> f a -> f b

fmap can be thought of in two ways:
1. it maps the function (a -> b) over the functor value a
2. it lifts the function (a -> b) into the functor f

If fmap was only for Either a its type would be fmap :: (b -> c) -> Either a b -> Either a c

IO is an instance of Functor like this:
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
-}

-- If fmap were limited to IO, its type would be fmap :: (a -> b) -> IO a -> IO b
{-
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
-}

-- An interesting instance of Functor is (->) r
-- The fmap type for this instance can be expressed with either of the following:
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
-- Mapping a function over a function produces a function. This is function composition.

-- r1 to r3 are equivalent and show how fmap relates to function composition.
r1 = fmap (*3) (+100) 1
r2 = (*3) `fmap` (+100) $ 1
r3 = (*3) . (+100) $ 1
-- and we can throw in a show function for good measure:
r4 = fmap (show . (*3)) (+100) 1

-- the functor laws ensure a functor behaves like a thing that can be mapped over:
-- 1. fmap id = id
-- 2. fmap (f . g) = fmap f . fmap g

-- here is an example of a functor instance that does not obey the laws:
data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

-- by mapping “multiparameter” functions over functor values, we get functor values that contain functions inside them

-- Here is the type class for the Applicative Functor:
{-
class (Functor f) => Applicative f where -- f is a type constructor that takes one type parameter
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}

-- The Applicative instance implementation for Maybe looks like this:
{-
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
-}

-- <*> is left-associative so the following two expressions are equivalent:
exp1 = pure (+) <*> Just 3 <*> Just 5
exp2 = (pure (+) <*> Just 3) <*> Just 5

-- given a function f that expects parameters that aren’t applicative values,
-- we can apply the function in the applicative style to applicative values x, y, ... as follows:
-- pure f <*> x <*> y <*> ...

-- but pure f <*> x is equivalent to fmap f x so we could have said:
-- fmap f x <*> y <*> ...

-- Control.Applicative exports a function called <$> which is fmap as an infix operator, so our final version is:
-- f <$> x <*> y <*> ...

-- combine values Just "johntra" and Just "volta" into one String inside a Maybe functor:
maybeActor = (++) <$> Just "johntra" <*> Just "volta"

-- a list is an instance of the Applicative Functor like so:
{-
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
-}
-- <*> is used with lists like this:
applylist = [(*0),(+100),(^2)] <*> [1,2,3]

-- if the functions take two parameters then we can apply them between two lists:
applylists = [(+),(*)] <*> [1,2] <*> [3,4]

-- another example
exp3 = (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]

-- lists are like nondeterministic computations. the following adds two nondeterministic values:
exp4 = (+) <$> [2,3] <*> [5,6,7]

-- the applicative style can often be used in place of list comprehensons. the following are equivalent:
expr5 = [ x*y | x <- [2,5,10], y <- [8,10,11]]
expr6 = (*) <$> [2,5,10] <*> [8,10,11]

-- IO is also an Applicative. its implementation is like so:
{-
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
-}

-- myAction prompts the user for two lines and returns the two lines concatenated:
myAction :: IO String
myAction = (++) <$> getLine <*> getLine

-- a function can be used as an applicative, but it's not clear why yet:
{-
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
-}
expr7 = (+) <$> (+3) <*> (*100) $ 5

-- ZipList applies each function in the first list with the corresponding value in the second list
-- unlike the standard list Applicative which results in every possible combination.
{-
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
-}
-- this is how you use zip lists in an applicative style:
zl1 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
zl2 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList (repeat 100)
zl3 = getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
zl4 = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"


-- consider the liftA2 function from Control.Applicative:
{-
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
-}

-- take a list of applicative values and return an applicative value that has a list as its result value
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

-- this evaluates to Just [1, 2]:
expr8 = sequenceA [Just 1, Just 2]
-- this evaluates to [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]:
expr9 = sequenceA [[1,2,3],[4,5,6]]
-- this evaluates to []
expr10 = sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]

-- sequenceA is useful when we have a list of functions and we want to
-- feed the same input to all of them and then view the list of results.

expr11 = sequenceA [(>4),(<10),odd] 7
-- is the same as
expr12 = map (\f -> f 7) [(>4),(<10),odd]