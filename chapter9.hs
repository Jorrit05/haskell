
-- data Op = Add | Sub | Mul | Div

-- instance Show Op where
--   show Add = "+"
--   show Sub = "-"
--   show Mul = "*"
--   show Div = "/"

-- valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y == 0

-- apply :: Op -> Int -> Int -> Int
-- apply Add x y = x + y
-- apply Sub x y = x - y
-- apply Mul x y = x * y
-- apply Div x y = x `div` y

-- data Expr = Val Int | App Op Expr Expr

-- instance Show Expr where
--   show (Val n) = show n
--   show (App o l r) = brak l ++ show o ++ brak r
--                      where
--                        brak (Val n) = show n
--                        brak e = "(" ++ show e ++ ")"

-- values :: Expr -> [Int]
-- values (Val n) = [n]
-- values (App _ l r ) = values l ++ values r

-- eval :: Expr -> [Int]
-- eval (Val n) = [n | n > 0]
-- eval (App o l r) = [apply o x y | x <- eval l,
--                                   y <- eval r,
--                                   valid o x y]

-- solution :: Expr -> [Int] -> Int -> Bool
-- solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- e :: Expr
-- e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))


-- choices :: [a] -> [[a]]
-- -- choices = concat . map perms . subs
-- choices xss = [p | xs <- subs xss, p <- perms xs]

subs :: [a] -> [[a]]
subs []= [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [ p  | x <- subs xs, p <- perms x]

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ =  True
isChoice (x:xs) ys | elem x ys = isChoice xs ys
                   | otherwise = False


removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne x (y:ys)
  | x == y = ys
  | otherwise = y : removeOne x ys
-- isChoice :: Eq a => [a] -> [a] -> Bool
-- isChoice [] _ = True
-- isChoice (x:xs) ys = x `elem` ys && isChoice xs (removeOne x ys)
