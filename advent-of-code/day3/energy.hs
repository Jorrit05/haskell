import System.IO
import Control.Monad



readInt :: String -> Int
readInt = read

-- myHead :: [String] -> Int -> Char
-- myHead xs = return xs !! 2
-- transpose matrix
-- fold over elke rij.

main = do
    -- fileContent is one large string.
    fileContent <- readFile "test.txt"
    -- Break up the string to a list using the 'words' function.
    -- convert to 'Int' and insert to a list
    let bitList =  words fileContent
    let a = map head bitList
    -- b <- foldr (!!(2)) bitList
    return a
