-- Num n => [n] -> n
myproduct [] = 1
myproduct (n:ns) = n * myproduct ns
-- main = myproduct [1,2,3]


quadruple x = double ( double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

n =  a `div` length xs
     where
        a = 10
        xs = [1,2,3,4,5]

myInit xs = reverse (tail (reverse xs))
myInit2 xs = take (length xs -1) xs
mycopy a = (a, a)
myapply f x = f x 

second :: [a] -> a
second xs = head (tail xs)

-- swap :: (a,a) -> (a,a)
swap :: x -> y -> (y,x)
swap x y = (y,x)

pair :: x -> y -> (x,y)
pair x y = (x,y)

double :: Int -> Int
double x = x*2 

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- twice :: f -> x -> (f -> x -> f x)
twice f x = f (f x)

odds n = map f [0..n-1]
         where f x = x*2 + 1

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs
-- halve xs = (take (length xs `div` 2) xs, reverse(take (length xs `div` 2) (reverse xs)))

third :: [a] -> a
third (_:_:x:_) = x
-- third xs = head (tail (tail xs))
-- third xs | (length xs >= 3) = xs !! 2
--          | otherwise = xs !! 1

safetail :: [a] -> [a]
-- safetail xs | length xs == 0 = []
--             | otherwise = tail xs
-- safetail xs = if length xs == 0 then []
--               else tail xs
safetail [] = []
safetail (_:xs) = xs

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True