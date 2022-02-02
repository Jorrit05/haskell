-- import Data.Map.Internal.Debug (Node1)
-- import Language.Haskell.TH.Syntax (occString, Exp)
-- -- import System.Win32 (RegInfoKey(values))
-- type Pos = (Int,Int)
-- type Assoc k v = [(k,v)]

-- find :: Eq k => k -> Assoc k v -> v
-- find k t = head [v |  (k', v) <- t, k == k']

-- data Move = North | South | East | West deriving Show

-- move :: Move -> Pos -> Pos
-- move North (x,y) = (x,y+1)
-- move South (x,y) = (x,y-1)
-- move East (x,y) = (x+1,y)
-- move West (x,y) = (x-1,y)

-- moves :: [Move] -> Pos -> Pos
-- moves ms p = foldl (flip move) p ms

-- rev :: Move -> Move
-- rev North = South
-- rev South = North
-- rev East = West
-- rev West = East

-- data Shape = Circle Float | Rect Float Float deriving Show

-- square :: Float -> Shape
-- square n = Rect n n

-- area :: Shape -> Float
-- area (Circle r) = pi * r^2
-- area (Rect x y) = x * y

-- safediv :: Int -> Int -> Maybe Int
-- safediv _ 0 = Nothing
-- safediv x y = Just (x `div` y)

-- safehead :: [a] -> Maybe a
-- safehead [] = Nothing
-- safehead xs = Just (head xs)

-- data Tree1 a = Leaf1 a | Node1 (Tree1 a) a (Tree1 a) deriving Show
-- t :: Tree1 Int
-- t = Node1 (Node1 (Leaf1 1) 3 (Leaf1 4)) 5
--          (Node1 (Leaf1 6) 7 (Leaf1 9))

-- occurs :: Ord a => a -> Tree1 a -> Bool
-- occurs x (Leaf1 y) = x == y
-- occurs x (Node1 l y r) | x == y = True
--                       | x < y = occurs x l
--                       | otherwise = occurs x r

-- flatten :: Tree1 a -> [a]
-- flatten (Leaf1 x) = [x]
-- flatten (Node1 l x r) = flatten l ++ [x] ++ flatten r

-- data Prop = Const Bool
--             | Var Char
--             | Not Prop
--             | And Prop Prop
--             | Imply Prop Prop
--  deriving Show


-- nat2int :: Nat -> Int
-- nat2int Zero = 0
-- nat2int (Succ n) = 1 + nat2int n

-- int2nat :: Int -> Nat
-- int2nat 0 = Zero
-- int2nat n = Succ (int2nat (n-1))

-- p1 :: Prop
-- p1 = And (Var 'A') (Not (Var 'A'))

-- p2 :: Prop
-- p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- p3 :: Prop
-- p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- p4 :: Prop
-- p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- type Subst = Assoc Char Bool

-- eval :: Subst -> Prop -> Bool
-- eval _ (Const b) = b
-- eval s (Var x) = find x s
-- eval s (Not p) = not (eval s p)
-- eval s (And p q) = eval s p && eval s q
-- eval s (Imply p q) = eval s p <= eval s q

-- vars :: Prop -> [Char]
-- vars (Const _) = []
-- vars (Var x) = [x]
-- vars (Not p) = vars p
-- vars (And p q) = vars p ++ vars q
-- vars (Imply p q) = vars p ++ vars q

-- bools :: Int -> [[Bool]]
-- bools 0 = [[]]
-- bools n = map (False:) bss ++ map (True:) bss
--           where bss = bools (n-1)


-- rmdups :: Eq a => [a] -> [a]
-- rmdups [] = []
-- rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- substs :: Prop -> [Subst]
-- substs p = map (zip vs) (bools (length vs))
--            where vs = rmdups (vars p)

-- isTaut :: Prop -> Bool
-- isTaut p = and [eval s p | s <- substs p]

-- data Expr = Val Int | Add Expr Expr

-- value :: Expr -> Int
-- value (Val n) = n
-- value (Add x y) = value x + value y

-- type Cont = [Op]

-- data Op = EVAL Expr | ADD Int

-- eval' :: Expr -> Cont -> Int
-- eval' (Val n) c = exec c n
-- eval' (Add x y) c = eval' x (EVAL y : c)

-- exec :: Cont -> Int -> Int
-- exec [] n = n
-- exec (EVAL y : c) n = eval' y (ADD n : c)
-- exec (ADD n : c) m = exec c (n+m)

-- value' :: Expr -> Int
-- value' e = eval' e []

data Nat = Zero | Succ Nat
            deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

one = Succ Zero
two = Succ (Succ Zero)

data Tree1 a = Leaf1 a | Node1 (Tree1 a) a (Tree1 a) deriving Show
t :: Tree1 Int
t = Node1 (Node1 (Leaf1 1) 3 (Leaf1 4)) 5
         (Node1 (Leaf1 6) 7 (Leaf1 9))




occurs' :: Ord a => a -> Tree1 a -> Bool
occurs' x (Leaf1 y) = x == y
occurs' x (Node1 l y r) | x == y = True
                      | x < y = occurs' x l
                      | otherwise = occurs' x r

occurs :: Ord a => a -> Tree1 a -> Bool
occurs x (Leaf1 y) = x == y
occurs x (Node1 l y r) =   case compare x y of
                            LT -> occurs x l
                            EQ -> True
                            GT -> occurs x r


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

y :: Tree Int
y = Node (Node (Node (Leaf 7) (Leaf 9)) (Leaf 4))
         (Node (Leaf 6) (Node (Leaf 7) (Node (Leaf 7) (Leaf 9))))

numberOfLeaves :: Tree a -> Int
numberOfLeaves (Leaf x) = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node l r) | abs (numberOfLeaves l - numberOfLeaves r) > 1 = False
                    | otherwise = True


halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance l) (balance r)
             where
               (l,r) = halves xs

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add a b) = g (folde f g a) (folde f g b)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- instance Eq a => Eq (Maybe a) where
--   Nothing == Nothing = True
--   Just x == Just y = x == y
--   _ == _ = False

-- instance Eq [a] where
--   [] == [] = True
--   (x:xs) == (y:ys) = x == y && xs == ys
--   _ == _ = False
