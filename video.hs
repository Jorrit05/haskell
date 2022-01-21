-- foldr :: (a -> b -> b) b -> [a] -> b
-- foldl :: (a -> b -> a) a -> [b] -> a

-- rev [1,2,3] -> [3,2,1]
rev :: [a] -> [a]
rev = foldl (\acc x  ->  x : acc) []

-- prefixes [1,2,3] -> [[1], [1,2], [1,2,3]]
prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : (map ((:) x) acc) ) []


func :: a -> [a] -> [[a]]
func x y = [x] : (map ((:) x) y)