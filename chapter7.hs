-- import Data.Char
-- import Data.List

type Bit = Int

-- bit2int :: [Bit] -> Int
-- bit2int = foldr (\x y -> x + 2 * y) 0
-- -- bit2int bits = sum [ w * b | (w,b) <- zip weights bits]
-- --                where weights = iterate (*2) 1

-- int2bin :: Int -> [Bit]
-- -- int2bin 0 = []
-- -- int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- make8 :: [Bit] -> [Bit]
-- make8 bits = take 8 (bits ++ repeat 0)

-- encode :: String -> [Bit]
-- encode = concat . map (make8 . int2bin . ord)

-- decode :: [Bit] -> String
-- decode = map (chr . bit2int) . chop8

-- transmit :: String -> String
-- transmit = decode . channel . encode

-- channel :: [Bit] -> [Bit]
-- channel = id

-- --Votes

-- votes :: [String]
-- votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- count :: Eq a => a -> [a] -> Int
-- count x = length . filter (==x)

-- rmdups :: Eq a => [a] -> [a]
-- rmdups [] = []
-- rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- result :: Ord a => [a] -> [(Int, a)]
-- result vs = sort [ (count x vs, x) | x <- rmdups vs ]

-- winner :: Ord a => [a] -> a
-- winner = snd . last . result

-- ballots :: [[String]]
-- ballots = [["Red", "Green"],
--             ["Green"],
--             ["Blue"],
--             ["Green", "Red", "Blue"],
--             ["Blue", "Green", "Red"]]

-- rmempty :: Ord a => [[a]] -> [[a]]
-- rmempty = filter (/= [])

-- elim :: Ord a => a -> [[a]] -> [[a]]
-- elim x = map (filter (/= x))

-- rank :: Ord a => [[a]] -> [a]
-- rank = map snd . result . map head

-- winner' :: Ord a => [[a]] -> a
-- winner' bs = case rank (rmempty bs) of
--                     [c] -> c
--                     (c:cs) -> winner' (elim c bs)
-- -- Exercises
-- -- [f x | x <- xs, p x]
-- -- func x = map (filter p x)
-- -- map f (filter p xs)

-- -- myall :: (a -> Bool) -> [Bool] -> Bool
-- myall p = and . map p

-- -- any' :: (a -> Bool) -> [Bool] -> Bool
-- any' p = or . map p

-- takeWhile' :: (a -> Bool) -> [a] -> [a]
-- takeWhile' _ [] = []
-- takeWhile' p (x:xs) | p x = x : takeWhile' p xs
--                     | otherwise = []

-- dropWhile' :: (a -> Bool) -> [a] -> [a]
-- dropWhile' _ [] = []
-- dropWhile' p (x:xs) | p x = dropWhile' p (tail xs)
--                     | otherwise = x : xs


-- filter' p = foldr (\x xs -> if p x then x : xs else xs) []

-- dec2int :: [Int] -> Int
-- dec2int = foldl (\x y -> 10*x + y) 0
-- -- a=(\x y -> 10*x + y)
-- -- 0 `a` 1 `a` 2
-- -- 12
-- -- 0 `a` 1 `a` 2 `a` 3
-- -- 123
-- -- 0 `a` 1
-- -- 1


-- -- foldl :: (a -> b -> a) -> a -> [b] -> a
-- -- foldl f v [] = v
-- -- foldl f v (x:xs) = foldl f (f v x) xs

-- curry' :: ((a,b) -> c) -> (a->b->c)
-- curry' f = \x y -> f (x,y)

-- uncurry' :: (a->b->c) -> ((a,b) -> c)
-- uncurry' f = \(x,y) -> f x y

-- unfold :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> [a]
-- unfold p h t x | p x = []
--                | otherwise = h x : unfold p h t (t x)

-- int2bin = unfold (== 0) (`mod` 2) (`div` 2)


-- chop8 :: [Bit] -> [[Bit]]
-- -- chop8 [] = []
-- -- chop8 bits = take 8 bits : chop8 (drop 8 bits)
-- chop8 = unfold null (take 8) (drop 8)

-- map' :: (a -> b) -> [a] -> [b]
-- -- map' :: (a -> b) -> [a] -> [b]
-- -- map' f xs = [f x | x <- xs]
-- -- map' f = foldr (\x xs -> f x : xs) []
-- map' f = unfold null (f . head) tail

-- iterate' :: (a -> a) -> a -> [a]
-- iterate' = unfold (const False) id


-- x = [f x | x <- xs, p x]

func :: (a -> b) -> [a] -> (a -> Bool) -> [b]
func f xs p = map f $ filter p xs

all' :: Eq a => (a -> Bool) -> [a] -> Bool
all' p xs = length (filter p xs) == length xs

all'' :: Eq a => (a -> Bool) -> [a] -> Bool
all'' p = and . map p

all''' :: Eq a => (a -> Bool) -> [a] -> Bool
all''' p = foldr (\x y -> p x && y) True

all'''' :: Eq a => (a -> Bool) -> [a] -> Bool
all'''' _ [] = True
all'''' p (x:xs) = p x && all'''' p xs

any' :: Eq a => (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) = p x || any' p xs

any'' :: Eq a => (a -> Bool) -> [a] -> Bool
any'' _ [] = False
any'' p (x:xs) | p x = True
               | otherwise = any'' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\acc elem -> 10 * acc + elem ) 0

-- fromDigits' :: [Int] -> Int
-- fromDigits' = foldl addDigit 0
--    where addDigit num d = 10*num + d

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f a b = f (a,b)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (a, b) = f a b

unfold :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)


int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)
-- int2bin 0 = []
-- int2bin n = n `mod` 2 : int2bin (n `div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (==[])  (take 8) (drop 8)
-- -- chop8 [] = []
-- -- chop8 bits = take 8 bits : chop8 (drop 8 bits)

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f0 f1 (x:xs) = f0 x : altMap f1 f0 xs

luhnDouble :: Int -> Int
luhnDouble x | double > 9 = double - 9
             | otherwise = double
              where
                  double = x*2

luhn' :: Int -> Int -> Int -> Int -> Bool
luhn' w x y z = result `mod` 10 == 0
                where
                    result = x + z + luhnDouble w + luhnDouble y


luhn :: [Int] -> Bool
luhn xs = sum  (altMap  luhnDouble id (reverse xs)) `mod` 10 == 0


-- luhn :: [Int] -> [Int]
-- luhn xs = altMap luhnDouble id xs
