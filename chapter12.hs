inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

-- instance Functor [] where
--     --fmap :: (a->b) -> f a -> f b
--     fmap = map

data Tree a = Leaf | Node (Tree a) a (Tree a)
                deriving Show

simple_tree  = Node (Node (Leaf) 2 (Leaf)) 5 (Leaf)

instance Functor Tree where
    --fmap :: (a->b) -> f a -> f b
     fmap _ Leaf = Leaf
     fmap g (Node l y r) = Node (fmap g l) (g y) (fmap g r)

-- instance Functor IO where
--     --fmap :: (a->b) -> IO a -> IO b
--     fmap g mx = do {
--                     x <- mx;
--                     return (g x);
--                 }

-- instance Functor ((->) a) where
--     -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
--     fmap = (.)


-- pure :: a -> f a
-- (<*>) :: f (a->b) -> f a -> f b

-- fmap0 :: a -> f a
-- fmap0 = pure

-- fmap1 :: ( a -> b) -> f a -> f b
-- fmap1 g x = pure g <*> x

-- class Functor f => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a->b) -> f a -> f b


-- instance Applicative Maybe where
--     -- pure :: a -> Maybe a
--     pure = Just

--     -- <*> :: Maybe (a->b) -> Maybe a -> Maybe b
--     Nothing <*> _ = Nothing
--     (Just g) <*> mx = fmap g mx

-- instance Applicative [] where
--     -- pure :: a -> [a]
--     pure x = [x]

--     -- <*> :: [a->b] -> [a] -> [b]
--     gs <*> xs = [g x | g <- gs, x <- xs]

data Expr = Val Int | Div Expr Expr


safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m

-- Exercise 3
-- instance Applicative ((->) a) where
--     -- pure :: b -> (a -> b)
--     pure = const

--     -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
--     g <*> h = \x -> g x (h x)

-- Exercise 4
newtype ZipList a = Z [a]
    deriving Show

instance Functor ZipList where
    --fmap :: (a->b) -> ZipList a -> ZipList b
    fmap g (Z []) = (Z [])
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    --pure :: a -> ZipList a
    pure x = (Z (repeat x))

    -- <*> :: ZipList (a->b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do
                    x <- st
                    return (g x)

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))

    -- <*> ST (a->b) -> ST a -> ST b
    stf <*> stx = do
                    f <- stf
                    a <- stx
                    return (f a)

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')