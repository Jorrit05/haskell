-- Welcome to the first exercise set of the Haskell Mooc! Edit this
-- file according to the instructions, and check your answers with
--
--   stack runhaskell Set1Test.hs
--
-- You can also play around with your answers in GHCi with
--
--   stack ghci Set1.hs
--
-- This set contains exercises on
--   * defining functions
--   * basic expressions
--   * pattern matching
--   * recursion

module Set1 where

import Mooc.Todo
import Language.Haskell.TH.Syntax (occString)
import Distribution.Simple (UserHooks(instHook))
import Control.Applicative ((<$>))
-- import Control
------------------------------------------------------------------------------
-- Ex 1: define variables one and two. They should have type Int and
-- values 1 and 2, respectively.
one :: Int
one = 1
two :: Int
two = 2
three :: Int
three = 3
------------------------------------------------------------------------------
-- Ex 2: define the function double of type Integer->Integer. Double
-- should take one argument and return it multiplied by two.

double :: Integer -> Integer
double x = x + x

------------------------------------------------------------------------------
-- Ex 3: define the function quadruple that uses the function double
-- from the previous exercise to return its argument multiplied by
-- four.

quadruple :: Integer -> Integer
quadruple x = double (double x)

------------------------------------------------------------------------------
-- Ex 4: define the function distance. It should take four arguments of
-- type Double: x1, y1, x2, and y2 and return the (euclidean) distance
-- between points (x1,y1) and (x2,y2).
--
-- Give distance a type signature, i.e. distance :: something.
--
-- PS. if you can't remember how the distance is computed, the formula is:
--   square root of ((x distance) squared + (y distance) squared)
--
-- Examples:
--   distance 0 0 1 1  ==>  1.4142135...
--   distance 1 1 4 5  ==>  5.0

distSqr :: Double -> Double -> Double
distSqr x y = (x - y)^2

distance :: Double -> Double -> Double -> Double -> Double
distance x y x' y' = sqrt (distSqr x' x + distSqr y' y)

------------------------------------------------------------------------------
-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.
--
-- Ps. have a look at the built in function "even"

eeny :: Integer -> String
eeny x | even x = "eeny"
       | otherwise = "meeny"

------------------------------------------------------------------------------
-- Ex 6: here's the function checkPassword from the course material.
-- Modify it so that it accepts two passwords, "swordfish" and
-- "mellon".

checkPassword :: String ->  String
checkPassword password | password == "swordfish" = "You're in."
                       | password == "mellon" = "You're in."
                       | otherwise = "ACCESS DENIED!"

------------------------------------------------------------------------------
-- Ex 7: A postal service prices packages the following way.
-- Packages that weigh up to 500 grams cost 250 credits.
-- Packages over 500 grams cost 300 credit + 1 credit per gram.
-- Packages over 5000 grams cost a constant 6000 credits.
--
-- Write a function postagePrice that takes the weight of a package
-- in grams, and returns the cost in credits.

postagePrice :: Int -> Int
postagePrice weight | weight <= 500 = 250
                    | weight > 500 && weight <= 5000 = 300 + weight
                    | otherwise = 6000

------------------------------------------------------------------------------
-- Ex 8: define a function isZero that returns True if it is given an
-- Integer that is 0, and False otherwise. Give isZero a type signature.
--
-- Use pattern matching! Don't use comparisons!
--
-- Ps. remember, the type of booleans in haskell is Bool

isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

------------------------------------------------------------------------------
-- Ex 9: implement using recursion a function sumTo such that
--   sumTo n
-- computes the sum 1+2+...+n

sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo x = x + sumTo (x-1)
-- sumTo x = sum [0..x]
-- sumTo x = last [0..x] + sumTo (x-1)

------------------------------------------------------------------------------
-- Ex 10: power n k should compute n to the power k (i.e. n^k)
-- Use recursion.

power :: Integer -> Integer -> Integer
power 0 _ = 0
power n 0 = 1
power n pwr = n * power n (pwr -1)

------------------------------------------------------------------------------
-- Ex 11: ilog3 n should be the number of times you can divide given
-- number by three (rounding down) before you get 0.
--
-- For example, ilog3 20 ==> 3 since
--   20/3 = 6.66 (gets rounded down to 6)
--   6/3 = 2
--   2/3 = 0.666 (gets rounded down to 0)
--
-- Use recursion to define ilog3. Use the function "div" for integer
-- division. It rounds down for you.
--
-- More examples:
--   ilog3 2 ==> 1
--   ilog3 7 ==> 2

ilog3 :: Integer -> Integer
ilog3 x = x `div` 3


-- all' f xs = length xs == length list2
--             where list2 = filter f xs
all' p = and . map p

filter' p = foldr (\x xs -> if p x then x : xs else xs ) []

dec2Int :: [Int] -> Int
dec2Int = foldl (\x xs -> 10*x + xs) 0

myF :: (Bool, Bool) -> Bool
myF = fst

any' f xs = not (null list2)
            where list2 = filter f xs

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x, y) -> f x y

int2bin :: Int -> [Int]
-- int2bin = unfold (==0) (`mod` 2) (`div` 2)
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 = unfold null (take 8) (drop 8)

-- map' :: ([Int] -> Int) -> [Int] -> [Int]
map' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f f2 [x] = [f x]
altMap f f2 (x:y:xs) = f x : f2 y : altMap f f2 xs
-- altMap f f2 (x:xs) = f x  : altMap f f2 xs

mapLuhn ::(Int->Bool) -> [Int] -> [Int]
mapLuhn p [] = []
mapLuhn p (x:xs) | p x = (x-9) : mapLuhn p xs
                 | otherwise = x : mapLuhn p xs

luhn :: [Int] -> Bool
luhn xs = sum reducedLuhnList `mod` 10 == 0
          where
            luhnList = reverse (altMap (+0) (*2) (reverse xs))
            reducedLuhnList = mapLuhn (>9) luhnList


data Nat = Zero | Succ Nat
       deriving Show

add' :: Nat -> Nat -> Nat
add' Zero  n = n
add' (Succ m) n = Succ (add' m n)

mult' :: Nat -> Nat -> Nat
mult' m Zero  = Zero
mult' m (Succ n) = add' m (mult' m n)

-- data Tree a = Leaf a | Node (Tree a) a (Tree a)
--        deriving Show

-- t :: Tree Int
-- t = Node (Node (Leaf 1 ) 3 (Leaf 4)) 5
--          (Node (Leaf 6 ) 7 (Leaf 9))

-- -- occurs :: Eq a => a -> Tree a -> Bool
-- -- occurs x (Leaf y) = x == y
-- -- occurs x (Node l y r) = x == y || occurs x l || occurs x r


-- occurs :: Ord a => a -> Tree a -> Bool
-- occurs x (Leaf y) = x == y
-- occurs x (Node l y r) = case compare x y of
--                             LT -> occurs x l
--                             GT -> occurs x r
--                             EQ -> True

-- data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
--        deriving Show

-- t2 :: Tree' Int
-- t2 = Node' (Node' (Leaf' 1 ) (Node' (Leaf' 4) (Leaf' 5)))
--            (Leaf' 4)

-- leafs :: Tree' a -> Int
-- leafs (Leaf' _) = 1
-- leafs (Node' l r) = leafs l + leafs r

-- balanced :: Tree' a -> Bool
-- balanced (Node' l r) = abs (leafs l - leafs r) <= 1


data Tree a = Leaf | Node (Tree a) a (Tree a)
       deriving Show

t :: Tree Int
t = Node (Node Leaf 3 Leaf) 5
         (Node Leaf 7 Leaf)


instance Functor Tree where
       -- fmap :: (a->b) -> Tree a -> Tree b
       fmap g Leaf = Leaf
       fmap g (Node l x r) =  Node (fmap g l) (g x) (fmap g r)



-- instance Functor ((->) a) where
--        fmap = (.)


newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
       --fmap :: (a->b) -> ZipList a -> ZipList b
       fmap g (Z xs) = todo

-- instance Applicative ZipList where
--        -- pure :: a -> ZipList a
--        pure x = todo

--        -- <*> :: ZipList (a->b) -> ZipList a -> ZipList b
--        (Z gs) <$> (Z xs) = todo


myCompare :: Ord a => [a] -> [a]
myCompare [a] = [a]
myCompare [] = []
myCompare (x:y:xs) | x < y = x : myCompare xs
                   | otherwise  = y : myCompare xs

