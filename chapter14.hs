import Data.Monoid
import Data.Foldable

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--   -- mempty :: (a,b)
--   mempty = (mempty, mempty)

--   (mappend) (x,y) (x',y') = (x,y) `mappend` (x',y')



-- instance (Monoid a, Monoid b) => Monoid (a->b) where
--   -- mempty :: a -> b
--   mempty = const mempty
--   -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
--   f1 `mappend` f2 = \x -> f1 x `mappend` f2 x

instance Foldable Maybe where
  fold Nothing = mempty
  fold (Just x) = x

  foldMap _ Nothing = mempty
  foldMap f (Just x) = f x

  foldr _ _ Nothing = mempty
  foldr f b (Just x) = f x b

  foldl _ _ Nothing = mempty
  foldl f b (Just y) = f b y


instance Traversable Maybe where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = fmap f (Just x)