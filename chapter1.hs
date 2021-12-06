rsort :: Ord a => [a] -> [a]
rsort [] = []
rsort (x:xs) = rsort larger ++ [x] ++ rsort smaller
               where
                larger = [a | a <- xs, a > x]
                smaller = [a | a <- xs, a <= x]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                larger = [a | a <- xs, a > x]
                smaller = [a | a <- xs, a < x]

