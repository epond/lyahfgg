import Data.Ratio
import Data.List (all)

-- ********** Study this is more detail later **********

-- let's model a nondeterministic value, attaching a probability to each possibility
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

-- as a functor the function is applied to each element in the list, leaving the probabilities as they are
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

-- now we can do things like negate all of the elements:
expr1 = fmap negate (Prob [(2,1%2),(4,1%4),(6,1%4)])

-- return is easy to define but what about >>=? consider this example:
thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [(Prob [('a',1%2),('b',1%2)], 1%4)
    ,(Prob [('c',1%2),('d',1%2)], 3%4)
    ]
-- since m >>= f always equals join (fmap f m) for monads, we can focus on join (ie. flatten)
-- the nested probabilities can be flattened by multiplying the outer with the inner Rational
multAll :: (Prob a, Rational) -> [(a, Rational)]
multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs

instance Monad Prob where
    return x = Prob [(x,1%1)]    -- default context has the single value of x as a certainty
    m >>= f = flatten (fmap f m) -- monad law lets us define >>= in terms of flatten and fmap
    fail _ = Prob []             -- the same approach as for lists

-- check that the monad laws hold:
-- 1. return x >>= f should be equal to f x
-- 2. m >>= return is no different than m
-- 3. f <=< (g <=< h) should be the same as (f <=< g) <=< h

-- now let's model a normal coin and a loaded (9/1) coin:
data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = simplerFlips $ do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

-- the result of getProb flipThree is [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40), (False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]

-- Exercise: how can all of the same outcomes be combined into one outcome?
simplerFlips :: Prob Bool -> Prob Bool
simplerFlips (Prob flips) = Prob [fls, tru]
    where fls = foldl (\(_, pacc) (x, p) -> if (not x) then (False, pacc + p) else (False, pacc)) (False, 0 % 1) flips
          tru = foldl (\(_, pacc) (x, p) -> if x then (True, pacc + p) else (True, pacc)) (True, 0 % 1) flips
