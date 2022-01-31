a = ['a', 'b', 'c'] :: [Char]
b = ('a', 'b', 'c') :: (Char, Char, Char)
c = [(False, '0'), (True, '1')] :: [(Bool, Char)]
d = ([False, True], ['0','1'] ) :: ([Bool], [Char])
e = [tail, init, reverse] :: [[a] -> [a]]


palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

twice' :: (a -> a) -> a -> a
twice' f x = f (f x)

double' :: Num a => a -> a
double' x = x * 2

pair' :: a -> b -> (a,b)
pair' x y = (x,y)

swap :: (a,b) -> (b,a)
swap  (x,y) = (y,x)

second :: [a] -> a
second xs = head (tail xs)