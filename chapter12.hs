-- import Control.Applicative
-- inc :: [Int] -> [Int]
-- inc [] = []
-- inc (n:ns) = n+1 : inc ns

-- sqr :: [Int] -> [Int]
-- sqr [] = []
-- sqr (n:ns) = n^2 : sqr ns

-- -- instance Functor [] where
-- --     --fmap :: (a->b) -> f a -> f b
-- --     fmap = map

-- instance Functor Tree where
--     --fmap :: (a->b) -> f a -> f b
--      fmap _ Leaf = Leaf
--      fmap g (Node l y r) = Node (fmap g l) (g y) (fmap g r)

-- -- instance Functor IO where
-- --     --fmap :: (a->b) -> IO a -> IO b
-- --     fmap g mx = do {
-- --                     x <- mx;
-- --                     return (g x);
-- --                 }

-- -- instance Functor ((->) a) where
-- --     -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
-- --     fmap = (.)


-- -- pure :: a -> f a
-- -- (<*>) :: f (a->b) -> f a -> f b

-- -- fmap0 :: a -> f a
-- -- fmap0 = pure

-- -- fmap1 :: ( a -> b) -> f a -> f b
-- -- fmap1 g x = pure g <*> x

-- -- class Functor f => Applicative f where
-- --     pure :: a -> f a
-- --     (<*>) :: f (a->b) -> f a -> f b


-- -- instance Applicative Maybe where
-- --     -- pure :: a -> Maybe a
-- --     pure = Just

-- --     -- <*> :: Maybe (a->b) -> Maybe a -> Maybe b
-- --     Nothing <*> _ = Nothing
-- --     (Just g) <*> mx = fmap g mx

-- -- instance Applicative [] where
-- --     -- pure :: a -> [a]
-- --     pure x = [x]

-- --     -- <*> :: [a->b] -> [a] -> [b]
-- --     gs <*> xs = [g x | g <- gs, x <- xs]

-- data Expr = Val Int | Div Expr Expr


-- safediv :: Int -> Int -> Maybe Int
-- safediv _ 0 = Nothing
-- safediv x y = Just (x `div` y)

-- eval :: Expr -> Maybe Int
-- eval (Val n) = Just n
-- eval (Div x y) = do n <- eval x
--                     m <- eval y
--                     safediv n m

-- -- Exercise 3
-- -- instance Applicative ((->) a) where
-- --     -- pure :: b -> (a -> b)
-- --     pure = const

-- --     -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
-- --     g <*> h = \x -> g x (h x)

-- -- Exercise 4
-- newtype ZipList a = Z [a]
--     deriving Show

-- instance Functor ZipList where
--     --fmap :: (a->b) -> ZipList a -> ZipList b
--     fmap g (Z []) = (Z [])
--     fmap g (Z xs) = Z (fmap g xs)

-- instance Applicative ZipList where
--     --pure :: a -> ZipList a
--     pure x = (Z (repeat x))

--     -- <*> :: ZipList (a->b) -> ZipList a -> ZipList b
--     (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]



data Tree a = Leaf | Node (Tree a) a (Tree a)
                deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

simpleTree  = Node (Node Leaf 2 Leaf) 5 Leaf


-- instance Applicative ((->) r) where
--   -- pure :: a -> (r -> a)
--   pure = const
--   -- (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
--   (<*>) f g x = f x (g x)

-- -- Exercise 4
newtype ZipList a = Z [a]
    deriving Show

instance Functor ZipList where
    --fmap :: (a->b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (repeat x)

    -- <*> :: ZipList (a->b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) =  Z [g x | (g,x) <- zip gs xs]

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
                deriving (Show)

x = Var 3
y = Val 6

instance Functor Expr where
    -- fmap :: ( a-> b) -> Expr a -> Expr b
    fmap _ (Val b) = Val b
    fmap f (Var a) = Var (f a)
    fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    _ <*> (Val x) = Val x
    Val x <*> _ = Val x
    Var f <*> Var x = Var (f x)
    Var f <*> Add x y = Add (fmap f x) (fmap f y)
    Add f g <*> x = Add (f <*> x) (g <*> x)


instance Monad Expr where
    return = pure

    -- (>>=) :: Expr a -> ( a -> Expr b) -> Expr b
    (>>=) (Val x)  _ = Val x
    (>>=) (Var x) f = f x
    (>>=) (Add a b) f = Add (a >>= f) (b >>= f)


-- z =  do x <- (\x -> Var (x*2)) 3
--         return x


type State = Int

newtype ST a = S (State -> (a, State))


-- z :: Num a => ST a
-- z = S (\s -> (1,s))


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
