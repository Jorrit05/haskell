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

processMovementAim ::(Int, Int, Int) ->  Movement -> (Int, Int, Int)
processMovementAim (horizontal, depth, aim) (Forward x) = (horizontal + x, depth + x*aim, aim)
processMovementAim (horizontal, depth, aim) (Down y)  = (horizontal, depth, aim + y)
processMovementAim (horizontal, depth, aim) (Up z)  = (horizontal, depth, aim - z)

processMovement :: Movement -> (Int, Int) -> (Int, Int)
processMovement (Forward x) (horizontal, depth) = (horizontal + x, depth)
processMovement (Up x) (horizontal, depth) = (horizontal, depth - x)
processMovement (Down x) (horizontal, depth) = (horizontal, depth + x)

getPuzzleTwoResult :: (Int, Int, Int) -> Int
getPuzzleTwoResult (horizontal, depth, _) = horizontal * depth

main = do
        -- fileContent is one large string.
        fileContent <- readFile "input.txt"
        -- Break up the string to a list using the 'lines' function.
        let movementList =  readMovement $ lines fileContent
        let puzzle1 = foldr processMovement (0,0) movementList
        let puzzle2 = foldl processMovementAim (0,0,0) movementList

        let answer1 = uncurry (*) puzzle1
        return (answer1 , getPuzzleTwoResult puzzle2)
