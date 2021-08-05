halve :: [a] -> ([a], [a])
-- halve xs = (take (length xs `div` 2) xs, reverse(take (length xs `div` 2) (reverse xs)))
-- halve xs  = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)
halve xs  = splitAt (length xs `div` 2) xs

third :: [a] -> a
-- third xs | length xs >= 3 = xs !! 2
third (_:_:x:_) = x

safetail :: [a] -> [a]
-- safetail xs | null xs = []
--             | otherwise = tail xs
-- safetail xs = if null xs then []
--               else tail xs
safetail [] = []
safetail (_:xs) = tail xs

concat1 :: [[a]] -> [a]
concat1 xss = [x | xs <- xss, x <- xs]