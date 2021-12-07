import System.IO
import Control.Monad

data Movement = Forward Int | Down Int | Up Int
    deriving Show

instance Eq Movement where
  (==) (Forward _) (Forward _) = True
  (==) (Down _) (Down _) = True
  (==) (Up _) (Up _) = True
  (==) (_) (_) = False

getValue :: Movement -> Int
getValue (Forward x) = x
getValue (Up x) = x
getValue (Down x) = x

readMovement :: [String] -> [Movement]
readMovement [] = []
readMovement (x:y:xs) | x == "forward" = Forward (read y) : readMovement xs
                      | x == "up" = Up (read y) : readMovement xs
                      | otherwise = Down (read y) : readMovement xs


getForward :: [Movement] -> [Int]
getForward [] = []
getForward (x:xs) | x == (Forward value) = value : getForward xs
                  | otherwise = getForward xs
               where
                   value = (getValue x)

getUp :: [Movement] -> [Int]
getUp [] = []
getUp (x:xs) | x == (Up value) = value : getUp xs
                  | otherwise = getUp xs
               where
                   value = (getValue x)

getDown :: [Movement] -> [Int]
getDown [] = []
getDown (x:xs) | x == (Down value) = value : getDown xs
               | otherwise = getDown xs
               where
                   value = (getValue x)

main = do
    -- fileContent is one large string.
    fileContent <- readFile "input.txt"
    -- Break up the string to a list using the 'words' function.
    let movementList =  readMovement $ words fileContent
    let forwards = sum $ getForward movementList
    let downs = sum $ getDown movementList
    let ups = sum $ getUp movementList

    return (forwards * (downs - ups))
