import Data.Monoid
import qualified Data.Foldable as F

-- the newtype keyword makes a new type out of an existing data type. eg.
{-
newtype ZipList a = ZipList { getZipList :: [a] }
-}

-- newtype is faster than data when wrapping and unwrapping an existing type
-- but is limited to only one value constructor which is limited to only one field

-- tuple cannot be made an instance of Functor in the same way that Maybe was
-- such that fmap applies its function to the first component of the tuple.

-- the second type parameter represents the type of the first component in the tuple.
newtype Pair b a = Pair { getPair :: (a, b) }

-- if fmap only worked on Pair its type would be fmap :: (a -> b) -> Pair c a -> Pair c b
instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)

-- this results in (5,2)
exp1 = getPair $ fmap (+4) $ Pair (1 :: Int, 2 :: Int)

-- type:    type synonym. gives another name to an already existing type. use to make your type signature more descriptive.
-- newtype: wrap an existing type in a new type. eases making instances of certain type classes.
-- data:    for making your own data type. no limit on the number of constructors or fields.

-- a monoid consists of:
-- 1. an associative binary function
-- 2. an identity value (aka zero value)

-- the definition from Data.Monoid is as follows:
{-
class Monoid m where                 -- only concrete types can be made instances of Monoid
    mempty :: m                      -- zero value
    mappend :: m -> m -> m           -- binary function
    mconcat :: [m] -> m              -- reduce a list into a single value
    mconcat = foldr mappend mempty
-}

-- numbers have more than one way to be a monoid, but newtype allows us to describe how a type implements a type class.
-- Data.Monoid exports the Product type so numbers can be a monoid in terms of multiplication:
{-
newtype Product a =  Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)
-}
-- the product of 2 and 3:
exp2 = getProduct $ Product 2 `mappend` Product 3
-- the product of a list:
exp3 = getProduct . mconcat . map Product $ [2,3,4]
-- the sum of 2 and 3:
exp4 = getSum $ Sum 2 `mappend` Sum 3
-- the sum of a list:
exp5 = getSum . mconcat . map Sum $ [2,3,4]
-- Bool can be an instance of Monoid with logical OR behaviour with the Any type:
exp6 = getAny . mconcat . map Any $ [False,True] -- normally you'd use the or function for this
-- and logical AND with All:
exp7 = getAll . mconcat . map All $ [False,True] -- mornally you'd use the and function for this

-- the Ordering instance of Monoid looks like this:
{-
instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
-}
-- it affords easy comparison by a list of criteria ordered by precendence.
-- so we can write a succinct function for comparing two strings first by length then alphabetically:
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)
-- if we wanted number of vowels to be the second most important criterion we could do this:
lengthCompare2 :: String -> String -> Ordering
lengthCompare2 x y = (length x `compare` length y) `mappend`
                     (vowels x `compare` vowels y) `mappend`
                     (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")

-- the standard Monoid for Maybe treats its type parameter as a Monoid:
{-
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
-}
-- this is useful for dealing with monoids as the result of computations that may have failed
exp8 = Just (Sum 3) `mappend` Just (Sum 4)

-- the First type allows for the type parameter of Maybe to not be a Monoid:
{-
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x
-}
exp9 = getFirst $ First Nothing `mappend` First (Just 'b')
exp10 = getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
-- similarly, Data.Monoid provides Last :
exp11 = getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]


-- the Data.Foldable type class is to fold what Functor is to map (and here imported qualified as F).
-- an easy way to make a type constructor an instance of Foldable is to implement the foldMap function:
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- back in chapter 7 we defined a Tree type (and demonstrated how it could be an instance of Functor):
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
-- this is how we make Tree an instance of Foldable:
instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r
-- say we have this tree:
testTree = Node 5
            (Node 3
                (Node 1 EmptyTree EmptyTree)
                (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
                (Node 8 EmptyTree EmptyTree)
                (Node 10 EmptyTree EmptyTree)
            )
-- then to test if the number three is in our tree:
hasThree = getAny $ F.foldMap (\x -> Any $ x == 3) testTree
-- we can easily turn the tree into a list:
treeAsList = F.foldMap (\x -> [x]) testTree