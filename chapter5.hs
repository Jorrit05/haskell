sumPower :: [Int] -> Int
sumPower xs = sum [x^2 | x <- xs]

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x,y) | x <- [0..x], y <- [0..y]]

square :: Int -> [(Int, Int)]
square n = [(x,y) | x <- [0..n], y <- [0..n], x /= y]

replicate' :: Int -> a -> [a]
replicate' n m = [m | x <- [1 .. n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n],
                     y <- [1..n],
                     z <- [1..n],
                     x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. (n-1)], n `mod` x == 0 ]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]

x = [(x,y) | x <- [1,2], y <- [3,4]]
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

result = [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

positions :: Eq a => a -> [a] -> [Int]
positions n xs = [i | (x, i) <- zip xs [0..],  x == n ]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [ v | (a,v) <- t, a == k ]

positions' :: Eq a => a -> [a] -> [Int]
positions' n xs =  find n $ zip xs [0..]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]