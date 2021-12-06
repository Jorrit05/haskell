-- Exercise set 4a:
--
-- * using type classes
-- * working with lists
--
-- Type classes you'll need
--  * Eq
--  * Ord
--  * Num
--  * Fractional
--
-- Useful functions:
--  * maximum
--  * minimum
--  * sort

module Set4a where

import Mooc.Todo
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array
import Control.Monad.Except
import Distribution.Simple.Utils (xargs)

------------------------------------------------------------------------------
-- Ex 1: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--   allEqual [] ==> True
--   allEqual [1,2,3] ==> False
--   allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual [a,b] = a == b
allEqual (x:y:xs) | x /= y = False
                  | otherwise = allEqual (y:xs)


------------------------------------------------------------------------------
-- Ex 2: implement the function distinct which returns True if all
-- values in a list are different.
--
-- Hint: a certain function from the lecture material can make this
-- really easy for you.
--
-- Examples:
--   distinct [] ==> True
--   distinct [1,1,2] ==> False
--   distinct [1,2] ==> True

distinct :: Eq a => [a] -> Bool
distinct xs = length xs == length (nub xs)

------------------------------------------------------------------------------
-- Ex 3: implement the function middle that returns the middle value
-- (not the smallest or the largest) out of its three arguments.
--
-- The function should work on all types in the Ord class. Give it a
-- suitable type signature.
--
-- Examples:
--   middle 'b' 'a' 'c'  ==> 'b'
--   middle 1 7 3        ==> 3

middle :: Ord a => a -> a -> a -> a
middle beg mid end = sort [beg, mid, end]  !! 1

------------------------------------------------------------------------------
-- Ex 4: return the range of an input list, that is, the difference
-- between the smallest and the largest element.
--
-- Your function should work on all suitable types, like Float and
-- Int. You'll need to add _class constraints_ to the type of range.
--
-- It's fine if your function doesn't work for empty inputs.
--
-- Examples:
--   rangeOf [4,2,1,3]          ==> 3
--   rangeOf [1.5,1.0,1.1,1.2]  ==> 0.5

rangeOf :: (Ord a, Num a) => [a] -> a
rangeOf [] = 0
rangeOf xs = end - begin
             where
               end = maximum xs
               begin = minimum xs

------------------------------------------------------------------------------
-- Ex 5: given a (non-empty) list of (non-empty) lists, return the longest
-- list. If there are multiple lists of the same length, return the list that
-- has the smallest _first element_.
--
-- (If multiple lists have the same length and same first element,
-- you can return any one of them.)
--
-- Give the function "longest" a suitable type.
--
-- Challenge: Can you solve this exercise without sorting the list of lists?
--
-- Examples:
--   longest [[1,2,3],[4,5],[6]] ==> [1,2,3]
--   longest ["bcd","def","ab"] ==> "bcd"


-- lowest :: (Ord a, Eq a) => [[a]] -> [a]
-- lowest [[]] = []
-- lowest (x:y:xs) | head x < head y = lowest (x : xs)
--                 | head x > head y = lowest (y : xs)
--                 | otherwise = lowest (x : xs)

-- longest :: (Ord a, Eq a) => [[a]] -> [a]
-- longest [[]] = []
-- longest xs = reduce longest_list
--              where
--                longest_list = [ x | x <- xs, length x == max_val]
--                max_val = maximum $ map length xs
              --  reduce :: (Ord a, Eq a) => [[a]] -> [a]
              -- --  reduce [a] = [a]
              --  reduce (x:y:zs) | head x < head y = reduce [x] ++ zs
              --                  | head x > head y = reduce [y] ++ zs
              --                  | head x == head y = x
-- longest (x:y:zs) | length x < length y = longest (y:zs)
--                  | length x > length y = longest (x:zs)

-- foldr (+) 0 [1,2,3] ==> foldr (+) 0 (1:2:3:[])
--                     ==> 1 + (foldr (+) 0 (2:3:[]))
--                     ==> 1 + (2 + (foldr (+) 0 (3:[])))
--                     ==> 1 + (2 + (3 + (foldr (+) 0 [])))
--                     ==> 1 + (2 + (3 + 0))

longest :: (Ord a) => [[a]] -> [a]
longest xs = foldr reducer (head xs) xs
  where
    reducer cur ans
      | length cur > length ans = cur
      | length cur < length ans = ans
      | head cur < head ans = cur
      | otherwise = ans

------------------------------------------------------------------------------
-- Ex 6: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
--
-- You'll need to add _class constraints_ to the type of incrementKey
-- to make the function work!
--
-- The function needs to be generic and handle all compatible types,
-- see the examples.
--
-- Examples:
--   incrementKey True [(True,1),(False,3),(True,4)] ==> [(True,2),(False,3),(True,5)]
--   incrementKey 'a' [('a',3.4)] ==> [('a',4.4)]

incrementKey :: (Eq k, Num v) => k -> [(k,v)] -> [(k,v)]
incrementKey _ [] = []
incrementKey key (x:xs) | fst x == key = (fst x, snd x +1) : incrementKey key xs
                        | otherwise = x : incrementKey key xs
-- incrementKey k = map (\kv -> if fst kv == k then (fst kv, snd kv + 1) else kv)

------------------------------------------------------------------------------
-- Ex 7: compute the average of a list of values of the Fractional
-- class.
--
-- There is no need to handle the empty list case.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

------------------------------------------------------------------------------
-- Ex 8: given a map from player name to score and two players, return
-- the name of the player with more points. If the players are tied,
-- return the name of the first player (that is, the name of the
-- player who comes first in the argument list, player1).
--
-- If a player doesn't exist in the map, you can assume they have 0 points.
--
-- Hint: Map.findWithDefault can make this simpler
--
-- Examples:
--   winner (Map.fromList [("Bob",3470),("Jane",2130),("Lisa",9448)]) "Jane" "Lisa"
--     ==> "Lisa"
--   winner (Map.fromList [("Mike",13607),("Bob",5899),("Lisa",5899)]) "Lisa" "Bob"
--     ==> "Lisa"

winner :: Map.Map String Int -> String -> String -> String
winner scores player1 player2 | player1_score >= player2_score = player1
                              | player1_score < player2_score = player2
                              | otherwise = "No clear winner"
                                where
                                  player1_score = Map.findWithDefault 0 player1 scores
                                  player2_score = Map.findWithDefault 0 player2 scores


------------------------------------------------------------------------------
-- Ex 9: compute how many times each value in the list occurs. Return
-- the frequencies as a Map from value to Int.
--
-- Challenge 1: try using Map.alter for this
--
-- Challenge 2: use foldr to process the list
--
-- Example:
--   freqs [False,False,False,True]
--     ==> Map.fromList [(False,3),(True,1)]

freqs :: (Eq a, Ord a) => [a] -> Map.Map a Int
freqs xs = foldr addToMap Map.empty xs
  where
    addToMap val cur = Map.alter (\_ -> Just (Map.findWithDefault 0 val cur + 1)) val cur


------------------------------------------------------------------------------
-- Ex 10: recall the withdraw example from the course material. Write a
-- similar function, transfer, that transfers money from one account
-- to another.
--
-- However, the function should not perform the transfer if
-- * the from account doesn't exist,
-- * the to account doesn't exist,
-- * the sum is negative,
-- * or the from account doesn't have enough money.
--
-- Hint: there are many ways to implement this logic. Map.member or
-- Map.notMember might help.
--
-- Examples:
--   let bank = Map.fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Mike" 20 bank
--     ==> fromList [("Bob",80),("Mike",70)]
--   transfer "Bob" "Mike" 120 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Lisa" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Lisa" "Mike" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]

-- withdraw "Bob" 80 bank
withdraw :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw account amount bank =
  case Map.lookup account bank of
    Nothing  -> bank                                   -- account not found, no change
    Just sum -> Map.insert account (sum-amount) bank   -- set new balance

withdraw' :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw' account amount = Map.adjust (\x -> x - amount) account
-- withdraw' account amount bank = Map.adjust (\x -> x-amount) account bank


transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank | not (Map.member from bank) = bank
                             | not (Map.member to bank) = bank
                             | amount <= 0 = bank
                             | Map.lookup from bank < Just amount = bank
                             | otherwise = Map.union (Map.fromList [(from, newFrom), (to, newTo)]) bank
                              where
                                newFrom = Map.findWithDefault 0 from bank - amount
                                newTo = Map.findWithDefault 0 to bank + amount


------------------------------------------------------------------------------
-- Ex 11: given an Array and two indices, swap the elements in the indices.
--
-- Example:
--   swap 2 3 (array (1,4) [(1,"one"),(2,"two"),(3,"three"),(4,"four")])
--         ==> array (1,4) [(1,"one"),(2,"three"),(3,"two"),(4,"four")]

swap :: Ix i => i -> i -> Array i a -> Array i a
swap i j arr = arr // [(i, arr ! j  ),(j, arr ! i)]

------------------------------------------------------------------------------
-- Ex 12: given an Array, find the index of the largest element. You
-- can assume the Array isn't empty.
--
-- You may assume that the largest element is unique.
--
-- Hint: check out Data.Array.indices or Data.Array.assocs

maxIndex :: (Ix i, Ord a) => Array i a -> i
maxIndex arr = maxList (assocs arr)
               where
                 maxList [(a,b)] = a
                 maxList (x:y:xs) | snd x > snd y = maxList (x:xs)
                                  | otherwise = maxList (y:xs)

-- maxIndex :: (Ix i, Ord a) => Array i a -> i
-- maxIndex arr = fst (foldr comparer (head arrList) arrList)
--   where
--     arrList = Data.Array.assocs arr
--     comparer v c = if snd v > snd c then v else c