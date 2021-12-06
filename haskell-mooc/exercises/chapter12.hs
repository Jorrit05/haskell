import Control.Monad.Writer.Lazy (Functor)
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

t :: Tree Int
t = Node (Node Leaf 3 Leaf) 5
         (Node Leaf 4 Leaf)


occurs :: Ord a => a -> Tree a -> Bool
occurs x Leaf = False
occurs x (Node l y r) = case compare x y of
                            LT -> occurs x l
                            GT -> occurs x r
                            EQ -> True

instance Functor Tree where
  -- fmap :: (a->b) -> Tree a -> tree b
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- instance Functor ((->) a) where
--   -- fmap :: (b -> c) -> (a ->b) -> (a ->c)
--   fmap = (.)
