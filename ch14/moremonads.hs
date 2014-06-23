import Data.Monoid
import Control.Monad.Writer
import Control.Applicative
import Control.Monad.State
import System.Random
import Data.List

-- the Writer monad is for values that have another value attached that acts as a sort of log value

-- consider this function that preserves the log when applying the function in the second parameter
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

-- applyLog takes care of the context, which in this case means the log.
-- this means the x inside the lambda can be just a simple value without context.
small   = (3, "Smallish gang.") `applyLog` isBigGang
big     = (30, "A freaking platoon.") `applyLog` isBigGang
tobin   = ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
bathcat = ("Bathcat", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))

-- replacing the String type with [c] allows for not just strings but any list
applyLog2 :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog2 (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- replacing the list with a monoid means we can think of the tuple as
-- a value with an accompanying monoid value which accumulates
applyLog3 :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog3 (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

beansWithDrink = ("beans", Sum 10) `applyLog3` addDrink
twoDrinks = ("dogmeat", Sum 5) `applyLog3` addDrink `applyLog3` addDrink

-- a monadic value acts like a simple value with an attached monoid

-- the Writer type is a newtype wrapper around the tuple used above
-- where w is the monoid and a is the value
{-
newtype Writer w a = Writer { runWriter :: (a, w) }
-}
-- its Monad instance implementation of >>= is the same as applyLog3 but using the Writer newtype
{-
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
-}
-- see how Writer's return function behaves with different monoids:
exp1 = runWriter (return 3 :: Writer String Int)        -- (3,"")
exp2 = runWriter (return 3 :: Writer (Sum Int) Int)     -- (3,Sum {getSum = 0})
exp3 = runWriter (return 3 :: Writer (Product Int) Int) -- (3,Product {getProduct = 1})

-- we can use do notation with Writer to multiply two numbers. note how the writer funtion is used
-- instead of the Writer value constructor to allow for future changes to the Writer implementation.
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])
-- the context here is a log of each number used in the calculation
multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"] -- has dummy value () as its result
    return (a*b)

-- adding logging to programs: >>= for Writer takes care of the logging for us
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- result value can be seen with: fst $ runWriter (gcd' 8 3)
-- log can be seen with: mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)

-- this function logs in reverse but it is inefficient because its use of ++ associates to the left instead of to the right
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

-- a difference list always supports efficient appending. it is a function of type [a] -> [a].
-- the difference list equivalent of the list [1,2,3] is the function \xs -> [1,2,3] ++ xs

-- let's create a DiffList newtype to make it easy to create monoid instances
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

-- now we can use DiffList to make the gcdReverse function more efficient:
gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcdReverse' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

-- test it with: mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse' 110 34


-- the function (->) r is a functor. the boxed function is applied first then the mapped function. it's function composition.
-- the function (->) r is an applicative functor. it allows us to operate on eventual results of functions as if we had their results.
-- the function (->) r is also a monad with implementations of return and >>=

-- say we have an integer, we want to separately *2 and +10 to it then add those results together
-- here is the applicative expression:
addAppl = (+) <$> (*2) <*> (+10)
-- and the same as a monadic expression:
addMonad = do
    a <- (*2)
    b <- (+10)
    return (a+b)
    
-- Conclusion: the function monad is also called the reader monad - all the functions read from a common source.


-- a stateful computation has a type like s -> (a, s) where a is the type of the result value and s is the type of the state.
-- the State monad looks like this:
{-
newtype State s a = State { runState :: s -> (a, s) }
instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState -- apply the new stateful computation, g, to the new state
-}

-- consider a stack implementation like so:
type Stack = [Int]
pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs) -- the state function wraps a function into the State newtype
push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

-- now we can us do notation to control state changes:
stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop -- this bind to a is unnecessary since it's not used. it could just be 'pop'
    pop

exp4 = runState stackManip [5,8,2,1]

-- we can introduce conditionals in the do
stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

-- since we're dealing with monadic values all round, these can be further glued together:
moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return () -- keeps state as it is and does nothing

-- now random generation can be modelled with state.
-- the random function from System.Random has type (RandomGen g, Random a) => g -> (a, g)
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)

tossThree = runState threeCoins (mkStdGen 33)


-- the error monad is Either, which is similar to Maybe, but might need an explicit type signature:
exp5 = Right 3 >>= \x -> return (x + 100) :: Either String Int

-- Exercise: from ch13, how can the error monad be used to remember the bird status when Pierre falls?
type Birds = Int
type Pole = (Birds, Birds)
landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise                    = Left $ "Pierre fell. Left side:" ++ show (left+n) ++ ", right side:" ++ show right
landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Right (left, right + n)
    | otherwise                    = Left $ "Pierre fell. Left side:" ++ show left ++ ", right side:" ++ show (right+n)
pierreSuccess = return (0, 0) >>= landLeft 1 >>= landRight 1 >>= landLeft 2
pierreFail =    return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)


-- liftM provides the ability of fmap from Functor. it takes the value from the monad, applies the function then puts it into a default context.
{-
liftM :: (Monad m) => (a -> b) -> m a -> m b
fmap :: (Functor f) => (a -> b) -> f a -> f b
-}

-- similarly, ap provides for the <*> function of Applicative. all monads are at least as strong as applicatives and functors.
{-
ap :: (Monad m) => m (a -> b) -> m a -> m b
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
-}

-- join flattens nested monadic values into a single monad
-- m >>= f always equals join (fmap f m) -- Scala flatMap!
{-
join :: (Monad m) => m (m a) -> m a
join mm = do
    m <- mm -- context of the monad is taken care of here
    m
-}
-- the result of join $ Just (Just 9) is Just 9
-- the result of join [[1,2,3],[4,5,6]] is [1,2,3,4,5,6]
-- the result of runWriter $ join (Writer (Writer (1, "aaa"), "bbb")) is (1,"bbbaaa")
-- the result of runState (join (state $ \s -> (push 10, 1:2:s))) [0,0,0] is ((),[10,1,2,0,0,0])

-- filter  :: (a -> Bool) -> [a] -> [a]
-- filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs
-- the result of powerset [1,2] is [[1,2],[1],[2],[]]

-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
-- now we can eg. fold with a condition, like add numbers only if they are all less than 10
binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9     = Nothing
    | otherwise = Just (acc + x)
-- the result of foldM binSmalls 0 [1,2,3,8,9] is Just 23
-- the result of foldM binSmalls 0 [1,2,3,8,9,10] is Nothing


-- let's make the rpn solver of chapter 10 implement graceful failure
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

-- the foldingFunction type signature in ch10 was foldingFunction :: [Double] -> String -> [Double]
foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((y * x):ys)
foldingFunction (x:y:ys) "+" = return ((y + x):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

-- let's generalise the knight problem of chapter 13 using monadic composition
-- a list of functions can be composed using id as the starting accumulator and . as the binary function
f = foldr (.) id [(+1),(*100),(+1)] -- the result of f 3 is 401
-- similary, monadic functions can be composed using return instead of id and <=< instead of .
{-
-- was in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)
-- was canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start
-}