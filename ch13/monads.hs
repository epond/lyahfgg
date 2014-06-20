-- functor:             something that can be mapped over. a value with context
--                      fmap :: (Functor f) => (a -> b) -> f a -> f b
-- applicative functor: preserved context over multiple normal function application
--                      (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
-- monad:               like applicative but function returns a value with a context
-- we want 'bind':      (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

-- here is the Monad typeclass:
{-
class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg
-}
-- return is the same as pure in the Applicative type class

-- here is how Maybe is an instance of Monad:
{-
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing
-}

-- examples of using >>= with Maybe:
exp1 = Just 3 >>= (\x -> Just(x+1))
exp2 = Nothing >>= \x -> Just (x+1)

-- Pierre's tightrope walking example
type Birds = Int
type Pole = (Birds, Birds)

landLeft' :: Birds -> Pole -> Pole
landLeft' n (left, right) = (left + n, right)

landRight' :: Birds -> Pole -> Pole
landRight' n (left, right) = (left, right + n)

-- suppose we define this helper function
x -: f = f x

-- now we can describe birds landing in a readable manner:
exp3 = (0, 0) -: landLeft' 1 -: landRight' 1 -: landLeft' 2
-- but situations that would cause Pierre to fall are not recognised, such as this:
exp4 = (0, 0) -: landLeft' 1 -: landRight' 4 -: landLeft' (-1) -: landRight' (-2)

-- the land functions need to be able to fail, so they must return a Maybe Pole
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

-- we use the monad functions return and >>=
exp5 = return (0, 0) >>= landLeft 1 >>= landRight 1 >>= landLeft 2
-- now the imbalance causes the result expression to be Nothing
exp6 = return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
-- the >> function is appropriate for simulating a banana peel on the wire:
exp7 = return (0, 0) >>= landLeft 1 >> Nothing >>= landRight 1

-- do notation. every line that isn't a let is a monadic value. the following are equivalent:
foo1 :: Maybe String
foo1 = Just 3   >>= (\x ->
       Just "!" >>= (\y ->
       Just (show x ++ y)))

foo2 :: Maybe String
foo2 = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

-- pierre's successful routine (equivalent to exp5):
routine1 :: Maybe Pole
routine1 = do
    start <- return (0, 0)
    first <- landLeft 1 start
    second <- landRight 1 first
    landLeft 2 second

-- pierre's unsuccessful routine:
routine2 :: Maybe Pole
routine2 = do
    start <- return (0, 0)
    first <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landLeft 1 second

-- the tighrope example lends itself more towards >>= than do notation because
-- each step relies specifically on the result of the previous step.

-- pattern matching in do notation
justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

-- when pattern matching fails in a do expression, the value is determined by the fail function of Monad

-- the Monad instance for lists looks like this:
{-
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs) -- flatMap!!!!
    fail _ = []
-}

-- when the Monad is list, the context is nondeterminism, ie. multiple values at the same time
listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n, ch)

-- the MonadPlus type class is for monads that can also act as monoids:
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

-- list can be made an instance of this type class as follows:
instance MonadPlus [] where
    mzero = []
    mplus = (++)

-- the guard basically says, "If this Boolean is False, then produce a failure right here.
-- Otherwise, make a successful value that has a dummy result of the empty tuple inside it."
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- the following three are equivalent
exp8 = [ x | x <- [1..50], '7' `elem` show x ]
exp9 = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x) -- like an if in a scala for expression
    return x

-- a knight's quest - a problem that really lends itself to being solved with nondeterminism
type KnightPos = (Int, Int) -- (column, row)
-- moveKnight takes the knight's current position an returns all of its possible next moves
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8]) -- makes sure the position lies on the board
    return (c', r')
-- in3 takes a start position and returns all possible positions after three moves
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- Monad laws:
-- Left identity: return x >>= f is the same thing as f x
-- Right Identity: m >>= return is the same as m
-- Associativity: (m >>= f) >>= g is the same as m >>= (\x -> f x >>= g)