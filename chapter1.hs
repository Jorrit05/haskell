-- rsort :: Ord a => [a] -> [a]
-- rsort [] = []
-- rsort (x:xs) = rsort larger ++ [x] ++ rsort smaller
--                where
--                 larger = [a | a <- xs, a > x]
--                 smaller = [a | a <- xs, a <= x]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                larger = [a | a <- xs, a >= x]
                smaller = [a | a <- xs, a < x]


-- double (double 2)
-- double (2 + 2)
-- (2 + 2) + (2 + 2)
-- (4) + (4)
-- 8

-- sum [x] = sum x:[] = x + sum [] = x + 0

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs


qsortReverse :: Ord a => [a] -> [a]
qsortReverse [] = []
qsortReverse (x:xs) = qsortReverse larger ++ [x] ++ qsortReverse smaller
               where
                larger = [a | a <- xs, a >= x]
                smaller = [a | a <- xs, a < x]
