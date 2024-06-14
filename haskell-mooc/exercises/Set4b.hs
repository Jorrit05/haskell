-- Exercise set 4b: folds

module Set4b where

import Mooc.Todo
import Data.Maybe

import Data.Char
import Data.List
------------------------------------------------------------------------------
-- Ex 1: countNothings with a fold. The function countNothings from
-- the course material can be implemented using foldr. Your task is to
-- define countHelper so that the following definition of countNothings
-- works.
--
-- Hint: You can start by trying to add a type signature for countHelper.
--
-- Challenge: look up the maybe function and use it in countHelper.
--
-- Examples:
--   countNothings []  ==>  0
--   countNothings [Just 1, Nothing, Just 3, Nothing]  ==>  2

countNothings :: [Maybe a] -> Int
countNothings = foldr countHelper 0
-- countNothings = foldr (\x acc -> if isNothing x then acc + 1 else acc) 0

countHelper :: Maybe a -> Int -> Int
countHelper may x | isNothing may = x + 1
                  | otherwise = x

-- countHelper Nothing x = x + 1
-- countHelper (Just _) x = x

------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold. Just like in the previous exercise,
-- define maxHelper so that the given definition of myMaximum works.
--
-- Examples:
--   myMaximum []  ==>  0
--   myMaximum [1,3,2]  ==>  3

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = foldr (\l acc -> if l > acc then l else acc ) x xs
-- myMaximum = foldr (\l acc -> if l > acc then l else acc ) 0

maxHelper :: Int -> Int -> Int
maxHelper x y | x>y = x
              | otherwise = y

------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold. Define
-- slHelper and slStart so that the given definition of sumAndLength
-- works. This could be used to compute the average of a list.
--
-- Start by giving slStart and slHelper types.
--
-- Examples:
--   sumAndLength []             ==>  (0.0,0)
--   sumAndLength [1.0,2.0,4.0]  ==>  (7.0,3)


sumAndLength :: [Double] -> (Double,Int)
-- sumAndLength = foldr slHelper (0.0, 0)
sumAndLength = foldr (\x acc -> (fst acc + x, snd acc +1)) (0.0, 0)

-- (\x acc -> )
slStart :: (Double, Int)
slStart = (0.0, 0)

slHelper :: (Num a, Num b) => a-> (a,b) -> (a, b)
slHelper x basecase = (fst basecase + x, snd basecase + 1)

------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold. Define concatHelper and
-- concatStart so that the given definition of myConcat joins inner
-- lists of a list.
--
-- Examples:
--   myConcat [[]]                ==> []
--   myConcat [[1,2,3],[4,5],[6]] ==> [1,2,3,4,5,6]

myConcat :: [[a]] -> [a]
-- myConcat xs = foldr concatHelper concatStart xs
myConcat = foldr (++) []

concatStart :: [a]
concatStart = []

concatHelper :: [a] -> [a] -> [a]
concatHelper x acc = x ++ acc

------------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a
-- fold. Implement largestHelper so that the given definition of largest works.
--
-- Examples:
--   largest [] ==> []
--   largest [1,3,2] ==> [3]
--   largest [1,3,2,3] ==> [3,3]

largest :: [Int] -> [Int]
largest = foldr largestHelper []

largestHelper :: Int -> [Int] -> [Int]
largestHelper x [] = [x]
largestHelper x xs | x > head xs = [x]
                   | x == head xs = x : xs
                   | otherwise = xs


------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold. Define
-- headHelper so that the given definition of myHead works.
--
-- Start by giving headHelper a type.
--
-- Examples:
--   myHead []  ==>  Nothing
--   myHead [1,2,3]  ==>  Just 1

myHead :: [a] -> Maybe a
myHead = foldr (\x acc -> Just x) Nothing

headHelper :: a -> b ->Maybe a
headHelper v _ = Just v

------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold. Define lasthelper
-- so that the given definition of myLast works.
--
-- Start by giving lastHelper a type.
--
-- Examples:
--   myLast [] ==> Nothing
--   myLast [1,2,3] ==> Just 3

myLast :: [a] -> Maybe a
myLast = foldr (\x acc -> if Data.Maybe.isNothing acc then Just x else acc)  Nothing

lastHelper :: a -> Maybe a -> Maybe a
lastHelper v c = if Data.Maybe.isNothing c then Just v else c

-- myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- myFoldr f b [] = b
-- myFoldr f b (x:xs) = f x (myFoldr f b xs)


-- (\x acc -> if )
-- myFoldr (\x acc -> if Data.Maybe.isNothing acc then Just x else acc) Nothing [1,2,3]

-- ()


data Pair a = Pair a a
  deriving Show

-- x :: Pair Int
-- x = Pair 10 20

instance Functor Pair where
    -- fmap :: (Pair a -> Pair b) -> Pair a -> Pair b
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure a = Pair a a
    (Pair f g) <*> (Pair a b) = Pair (f a) (g b)
    -- (Pair a1 b1) <*> (Pair a2 b2) = Pair (a1 a2) (b1 b2)

instance Foldable Pair where
    foldr f basecase (Pair x y) = f x (f y basecase)

-- instance Monad Pair where
--     -- (>>=) :: Pair a -> (a -> Pair b) -> Pair b
--     return = pure
--     (>>=) = (Pair a b) >>= f = let Pair a' _ = f a
--                                     Pair _ b' = f b
--                                in  Pair a' b'

instance Monad Pair where
  return a = Pair a a
  (Pair a b) >>= f = let Pair a' _ = f a
                         Pair _ b' = f b
                     in  Pair a' b'




-- main = do
--           x <- Pair 2 10
--           y <- Pair 3 9
--           return (x+y)

-- main = do line <- getLine
--           let line' = reverse line
--           putStrLn $ "You said " ++ line' ++ " backwards!"
--           putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"

-- main = do line <- fmap reverse getLine
--           putStrLn $ "You said " ++ line ++ " backwards!"
--           putStrLn $ "Yes, you really said " ++ line ++ " backwards!"



-- main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
--           putStrLn line

-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))

type Birds = Int
type Pole = (Birds, Birds)

i :: Pole
i = (0,0)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) | abs ((left + n) - right) < 4 = Just (n + left, right)
                         | otherwise  = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) | abs (left - (right + n)) < 4 = Just (left, right + n)
                          | otherwise  = Nothing



main = do x <- landRight 2 (0,0)
          y <- landLeft 3 x
          return y