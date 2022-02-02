import System.IO
import Data.List
import Data.Char

scanString :: String -> [Int]
scanString = map digitToInt

countDigits :: [Int] -> (Int, Int)
countDigits xs | ones > zeros = (1, 0)
               | otherwise  = (0, 1)
               where
                   ones = length $ filter (==1) xs
                   zeros = length $ filter (==0) xs

bit2int :: [Int] -> Int
bit2int = foldr (\x y -> x + 2 * y) 0

getBinaries :: [(Int,Int)] -> (Int,Int)
getBinaries xs = (bit2int . reverse $ map fst xs, bit2int . reverse $ map snd xs)

main = do
    -- fileContent is one large string.
    fileContent <- readFile "input.txt"
    -- Break up the string to a list using the 'words' function.
    -- convert to 'Int', insert to a multidimensional list and transpose.
    let bitList =  transpose $ map scanString $ words fileContent
    let puzzle1 = getBinaries $ map countDigits bitList
    return (uncurry (*) puzzle1)
