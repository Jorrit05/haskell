import System.IO
import Control.Monad

data Movement = Forward Int | Down Int | Up Int
    deriving (Show, Ord, Eq, Read)

getValue :: Movement -> Int
getValue (Forward x) = x
getValue (Up x) = x
getValue (Down x) = x

strToMovement :: String -> Movement
strToMovement str =  case words str of
                        ["forward", a] -> Forward (read a)
                        ["up", a] -> Up (read a)
                        ["down", a] -> Down (read a)
                        _ -> error "ohoh"

readMovement :: [String] -> [Movement]
readMovement = map strToMovement

processMovementAim :: Movement -> (Int, Int, Int) -> (Int, Int, Int)
processMovementAim (Forward x) (horizontal, depth, aim) = (horizontal + x, depth + x*aim, aim)
processMovementAim (Down x) (horizontal, depth, aim) = (horizontal, depth, aim + x)
processMovementAim (Up x) (horizontal, depth, aim) = (horizontal, depth, aim - x)

processMovement :: Movement -> (Int, Int) -> (Int, Int)
processMovement (Forward x) (horizontal, depth) = (horizontal + x, depth)
processMovement (Up x) (horizontal, depth) = (horizontal, depth - x)
processMovement (Down x) (horizontal, depth) = (horizontal, depth + x)

processListAim :: [Movement] -> (Int, Int, Int)
processListAim = foldr processMovementAim (0,0,0)

processList :: [Movement] -> (Int, Int)
processList = foldr processMovement (0,0)

getPuzzleTwoResult :: (Int, Int, Int) -> Int
getPuzzleTwoResult (horizontal, depth, aim) = horizontal * depth

main = do
        -- fileContent is one large string.
        fileContent <- readFile "input.txt"
        -- Break up the string to a list using the 'lines' function.
        let movementList =  readMovement $ lines fileContent
        let puzzle1 = processList movementList
        let puzzle2 =  processListAim movementList

        let answer1 = uncurry (*) puzzle1
        return (answer1 , getPuzzleTwoResult puzzle2)
