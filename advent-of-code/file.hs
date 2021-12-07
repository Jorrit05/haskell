import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
-- import Data.Attoparsec.Text

-- convertToInt :: Text.Text -> Int
-- convertToInt xs = return (parseOnly xs)


myCompare :: Ord a => [a] -> [a]
myCompare [a] = []
myCompare [] = []
myCompare (x:y:xs) | x > y = x : myCompare (y:xs)
                   | x == y = myCompare (y:xs)
                   | y > x
                   | otherwise  = y : myCompare (y:xs)



main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    return $ length (myCompare ls)


-- main = return compareLs ["1", "3"]
-- main = return x
--         where x = getList

-- destutter::[Integer] -> [Integer]
-- destutter [] = []
-- destutter (fst:snd:t) | fst == snd = destutter (snd : t)
-- destutter (h:t) = [h] ++ (destutter t)
-- outputMap :: String -> IO ()
-- outputMap = mapM_ putStrLn . lines