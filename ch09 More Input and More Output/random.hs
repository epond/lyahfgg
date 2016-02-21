import System.Random

-- define a function that given a random generator, tosses three coins
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

-- let's try it out
coinToss = threeCoins (mkStdGen 23)

-- we could have used the randoms function
coinToss2 = take 3 $ randoms (mkStdGen 23) :: [Bool]

-- randoms is implemented like this:
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

-- we can't get the generator back because the list is infinite, but we could if it was finite:
finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (value:restOfList, finalGen)

-- this is how you'd use the above
coinToss3 = finiteRandoms 3 (mkStdGen 23) :: ([Bool], StdGen)

-- get a random value in range:
rollDie = randomR (1,6) (mkStdGen 359353) :: (Int, StdGen)
-- get a stream of random values in a range:
secret = take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]

