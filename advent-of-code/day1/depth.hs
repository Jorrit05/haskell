import System.IO
import Control.Monad

compareDepth :: Ord a => [a] -> [a]
compareDepth [a] = []
compareDepth [] = []
compareDepth (x:y:xs) | y > x = y : compareDepth (y:xs)
                      | otherwise  = compareDepth (y:xs)

getThreeSum :: (Integral) a => [a] -> [a]
getThreeSum [_ , _] = []
getThreeSum xs = som : getThreeSum (tail xs)
                 where
                    som = sum $ take 3 xs

main = do
    -- fileContent is one large string.
    fileContent <- readFile "input.txt"
    -- Break up the string to a list using the 'words' function.
    -- convert to 'Int' and insert to a list
    let depthList =  map readInt $ words fileContent
    let sum_depth_list = getThreeSum depthList
    let assignment_1 = (length $ compareDepth depthList)
    let assignment_2 = (length $ compareDepth sum_depth_list)
    return (assignment_1, assignment_2)

readInt :: String -> Int
readInt = read
