-- (2^3)*4
-- (2*3)+(4*5)
-- 2+(3*(4^5))

n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

last' :: [a] -> a
last' = head . reverse


init' :: [a] -> [a]
init' = reverse . drop 1 . reverse

