import Data.Char
import Data.List

type Bit = Int

bit2int :: [Bit] -> Int
bit2int = foldr (\x y -> x + 2 * y) 0
-- bit2int bits = sum [ w * b | (w,b) <- zip weights bits]
--                where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bit2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

--Votes

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [ (count x vs, x) | x <- rmdups vs ]

winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots = [["Red", "Green"],
            ["Green"],
            ["Blue"],
            ["Green", "Red", "Blue"],
            ["Blue", "Green", "Red"]]

rmempty :: Ord a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Ord a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                    [c] -> c
                    (c:cs) -> winner' (elim c bs)
-- Exercises
-- [f x | x <- xs, p x]
-- func x = map (filter p x)
-- map f (filter p xs)

-- myall :: (a -> Bool) -> [Bool] -> Bool
myall p = and . map p

-- any' :: (a -> Bool) -> [Bool] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p (tail xs)
                    | otherwise = x : xs

-- map' :: (a -> b) -> [a] -> [b]
-- map' f xs = [f x | x <- xs]
map' f = foldr (\x xs -> f x : xs) []

filter' p = foldr (\x xs -> if p x then x : xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0
-- 0 + 10*1 + (10*2) + (10*3)