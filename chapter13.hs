import Data.Monoid
import Data.Foldable

-- exercise 1
-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--     -- mempty (a,b)
--     mempty = (mempty,mempty)

--     -- mappend :: (a,b) -> (a,b) -> (a,b)
--     (x1,y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2)


-- exercise 2
-- instance (Monoid a, Monoid b) => Monoid (a -> b) where
--     -- mempty (a -> b)
--     mempty = const mempty

--     -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
--     fl `mappend` fr = \x -> fl x `mappend` fr x

-- -- exercise 3
-- instance Foldable Maybe where
--     -- fold :: Monoid a => Maybe a -> a
--     fold Nothing = mempty
--     fold (Just a) = a

--     foldMap _ Nothing = mempty
--     foldMap f (Just a) = f a


-- exercise 4
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

x :: Tree Int
x = Node (Leaf) 9 (Node (Leaf) 10 (Leaf))

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node l x r) =  Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold Leaf = mempty
    fold (Node l x r) = fold l `mappend` x `mappend` fold r

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap _ Leaf = mempty
    foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ b Leaf = b
    foldr f b (Node l x r) = foldr f (foldr f (f x b) l) r

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldMap (\x -> if f x then [x] else mempty)
