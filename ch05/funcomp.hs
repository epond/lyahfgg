-- the $ function, or function application operator lets us write fewer parentheses.
-- normal function application has high precedence, but $ has the lowest. the following are equivalent:
sum1 = sum (filter (> 10) (map (*2) [2..10]))
sum2 = sum $ filter (> 10) $ map (*2) [2..10]

-- $ also allows us to map function application over a list of functions
mapfunc = map ($ 3) [(4+), (10*), (^2), sqrt]
-- mapfunc = [7.0,30.0,9.0,1.7320508075688772]

-- the dot operator performs function composition (right associative). the following are equivalent:
negSumTail1 = \xs -> negate (sum (tail xs))
negSumTail2 = negate . sum . tail

-- multiple parameters can be handled by partially applying each function so that it takes one parameter:
sumRepMax1 = sum (replicate 5 (max 6.7 8.9)) -- is the same as
sumRepMax2 = (sum . replicate 5) (max 6.7 8.9) -- is the same as
sumRepMax3 = sum . replicate 5 $ max 6.7 8.9

-- rewrite an expression with a lot of parentheses by writing out the innermost function and its parameters,
-- put a $ before it then compose all prior functions without their last parameter:
bigExpr1 = replicate 2 (product (map (*3) (zipWith max [1,2] [4,5]))) -- is the same as
bigExpr2 = replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]

-- point-free style is when a function parameter is eliminated on both sides of the equals sign.
-- in the example below function composition allows us to use the point-free style
fn1 x = ceiling (negate (tan (cos (max 50 x)))) -- is the same as
fn2 = ceiling . negate . tan . cos . max 50

-- a previous example simplified using function composition:
oddSquareSum1 = sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) -- is the same as
oddSquareSum2 = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]