main = do
    contents <- readFile "input.txt"
    print $ splitLines contents

splitLines :: String -> [String]
splitLines input = splitString input ""

splitString :: String -> String -> [String]
splitString "" word = [word]
splitString ('\n':xs) word = [word] ++ splitString xs ""
splitString (x:xs) word = splitString xs (word ++ [x])

data Direction = North | East | South | West
data Position = Position Int Int Direction

moveForward :: Position -> Position
moveForward (Position x y North) = Position x (y+1) North
moveForward (Position x y East) = Position (x+1) y East
moveForward (Position x y South) = Position x (y-1) South
moveForward (Position x y West) = Position (x-1) y West

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North
