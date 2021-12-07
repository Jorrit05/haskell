import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

compareDepth :: Ord a => [a] -> [a]
compareDepth [a] = [a]
compareDepth [] = []
compareDepth (x:y:xs) | y > x = y : compareDepth (y:xs)
                   | otherwise  = compareDepth (y:xs)

main = do
    depthList <- fmap Text.lines (Text.readFile "input.txt")
    return $ length (compareDepth depthList)
