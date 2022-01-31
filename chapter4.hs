-- halve :: [a] -> ([a], [a])
-- -- halve xs = (take (length xs `div` 2) xs, reverse(take (length xs `div` 2) (reverse xs)))
-- -- halve xs  = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)
-- halve xs  = splitAt (length xs `div` 2) xs

-- third :: [a] -> a
-- -- third xs | length xs >= 3 = xs !! 2
-- third (_:_:x:_) = x

-- safetail :: [a] -> [a]
-- -- safetail xs | null xs = []
-- --             | otherwise = tail xs
-- -- safetail xs = if null xs then []
-- --               else tail xs
-- safetail [] = []
-- safetail (_:xs) = tail xs

-- concat1 :: [[a]] -> [a]
-- concat1 xss = [x | xs <- xss, x <- xs]

-- luhnDouble :: Int -> Int
-- luhnDouble x = if nr > 9 then nr - 9 else nr
--                 where
--                     nr = x * 2

-- luhn :: Int -> Int -> Int -> Int -> Bool
-- luhn a b c d = (x + b + y + d) `mod` 10 == 0
--                 where
--                     x = luhnDouble a
--                     y = luhnDouble c


halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

third :: [a] -> a
third = head . tail . tail

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' [_, _, x] = x

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' [] = []
safetail' (_:xs) = xs

safetail'' :: [a] -> [a]
safetail'' xs | null xs = []
              | otherwise = tail xs

(|||) :: Bool -> Bool -> Bool
(|||) True _ = True
(|||) _ True = True
(|||) _ _ = False

(||||) :: Bool -> Bool -> Bool
(||||) False False = False
(||||) _ _ = True

(|||||) :: Bool -> Bool -> Bool
(|||||) a b | a == b = a
            | otherwise = True

(&&&) :: Bool -> Bool -> Bool
a &&& b =
  if a == True then
      if b == True then True else False
  else False

(&&&&) :: Bool -> Bool -> Bool
True &&&& b = if b == True then True else False
False &&&& _ = False


mult :: Num a => a -> a -> a -> a
-- mult x y z = x*y*z
mult = (\x y z -> x * y * z)


luhnDouble :: Int -> Int
luhnDouble x | double > 9 = double - 9
             | otherwise = double
              where
                  double = x*2


luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = result `mod` 10 == 0
                where
                    result = x + z + luhnDouble w + luhnDouble y