import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
-- import Data.Attoparsec.Text

-- convertToInt :: Text.Text -> Int
-- convertToInt xs = return (parseOnly xs)


compareDepth :: Ord a => [a] -> [a]
compareDepth [a] = [a]
compareDepth [] = []
compareDepth (x:y:xs) | y > x = y : compareDepth (y:xs)
                   | otherwise  = compareDepth (y:xs)

main = do
    depthList <- fmap Text.lines (Text.readFile "input.txt")
    return $ length (compareDepth depthList)


-- main = return compareLs ["1", "3"]
-- main = return x
--         where x = getList

-- destutter::[Integer] -> [Integer]
-- destutter [] = []
-- destutter (fst:snd:t) | fst == snd = destutter (snd : t)
-- destutter (h:t) = [h] ++ (destutter t)
-- outputMap :: String -> IO ()
-- outputMap = mapM_ putStrLn . lines