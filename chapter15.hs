import Control.Monad.ST (fixST)
a = 1+ (2*3) -- inner redex = (2*3)
b = (1+2) * (2+3) -- innermost: (1+2) &  (2+3)
c = fst (1+2,2+3) -- inner most = fst. other redex (1+2)

d = (\x -> 1 + x) (2*3) -- lambda is innermost, (2*3) is outermost

-- fst (1+2, 1+3)
-- apply fst:
-- 1+3
-- apply +
-- 4

mult :: Integer -> Integer -> Integer
mult = \x -> (\y -> x * y)

-- mult 2 3
-- apply mult
-- 1: \x -> (\y -> x * y) 2 3
-- first part lambda
-- 2: (\y -> 2 * y) 3
-- second part lambda
-- 3: 2 * 3
-- 4: 6

fibs :: [Int]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeat' :: Tree a -> [Tree a]
repeat' x =  xs where xs = x:xs

-- repeat' x = xs where xs = x:xs

sqroot :: Double -> Double
sqroot n = snd . head $ dropWhile pred (zip it (tail it))
  where
    approx = 0.00001
    pred (x,y) = abs (x - y) > approx
    it = iterate next n
    next a = (a + n / a) / 2
