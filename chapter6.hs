-- import Data.List
fac :: Int -> Int
-- fac n = product [1..n]
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

insert' :: Ord a => a -> [a] -> [a]
insert' x []               = [x]
insert' x (y:ys) | x <= y  = x : y : ys
                 | otherwise = y : insert' x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert' x (isort xs)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

even' :: Int -> Bool
even' 0 = True
even' n = odd (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even (n-1)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

product' :: Num a => [a] -> a 
product' [] = 1
product' (n:ns) = n * product' ns

init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- (^) :: Int -> Int -> Int
-- m ^ 0 = 1
-- m ^ n = m * (m^(n-1))

euclid :: Int -> Int -> Int
euclid m n | m == n = m 
           | m < n = euclid m (n - m) 
           | n < m = euclid (m - n) n


and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == False = False
            | x == True = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs  

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n m = [m] ++ replicate' (n-1) m

