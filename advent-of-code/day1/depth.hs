-- import qualified Data.Text    as Text
-- import qualified Data.Text.IO as Text

import System.IO
import Control.Monad

compareDepth :: Ord a => [a] -> [a]
compareDepth [a] = []
compareDepth [] = []
compareDepth (x:y:xs) | y > x = y : compareDepth (y:xs)
                   | otherwise  = compareDepth (y:xs)

main = do
    -- fileContent is one large string.
    fileContent <- readFile "input.txt"
    -- Break up the string to a list using the 'words' function.
    -- convert to 'Int' and insert to a list
    let depthList =  map readInt $ words fileContent

    return (length $ compareDepth depthList)

readInt :: String -> Int
readInt = read