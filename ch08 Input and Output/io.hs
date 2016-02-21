import Data.Char

-- 'do' lets us glue many io actions into a single io action.
-- <- is for performing an io action and binding its result to a name.
{-
main = do
    putStrLn "Hello, what's your name?"
    -- <- can only be done inside an io action, in this case main.
    -- if we want to deal with impure data, we must do it in an impure environment.
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
-}

-- 'let' lets us give names to normal values inside io actions, similar to let in list comprehensions
{-
main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " "
                      ++ bigLastName
                      ++ ", how are you?"
-}

-- 'return' makes an io action out of a pure value, sort of the opposite of <-
{-
main = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b
-}

-- 'print' is just 'putStrLn . show' and is what GHCi uses to display values on the terminal
{-
main = do
    print True
    print 2
    print "haha"
    print 3.2
    print [3,4,3]
-}

-- 'when' (Control.Monad) takes a Bool and an io and returns the io if the Bool is true, otherwise return ()
{-
main = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input
-}

-- 'sequence' takes a list of io and returns an io that performs them, resulting in a list of results.
-- the following are equivalent:
{-
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
-}
{-
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
-}

-- mapping a function that returns an io over a list and then sequencing it is common.
-- 'mapM' takes a function and a list, maps the function over the list, and then sequences it.
-- 'mapM_' does the same thing but throws away the result because we probably don't care about it.
-- 'mapM print [1,2,3]' is equivalent to 'sequence $ map print [1,2,3]' but we usually use 'mapM_ print [1,2,3]'

-- 'forever' (Control.Monad) takes an io and returns an io that repeats the io forever

-- 'forM' (Control.Monad) is like mapM but the parameters are switched. sometimes this can help readability.