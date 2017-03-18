
main = do
    contents <- readFile "input.txt"
    print $ perform Nothing $ map convertStringToInstruction $ splitLines contents


splitLines :: String -> [String]
splitLines input = splitString input '\n' ""

splitString :: String -> Char -> String -> [String]
splitString "" _ word = [word]
splitString (x:xs) delimiter word = if (x == delimiter) then [word] ++ splitString xs delimiter "" else splitString xs delimiter (word ++ [x])

data Instruction = TurnRight | TurnLeft | Move | Report | Place Position | Invalid deriving (Eq, Show)
data Direction = North | East | South | West deriving (Eq, Show)
data Position = Position Int Int Direction deriving (Eq, Show)

width = 5
height = 5

isValidPosition :: Position -> Bool
isValidPosition (Position x y _) = x >= 0 && x < width && y >= 0 && y < height

makeValidMove :: Position -> Position
makeValidMove position = if isValidPosition $ moveForward position then moveForward position else position

moveForward :: Position -> Position
moveForward (Position x y North) = Position x (y+1) North
moveForward (Position x y East) = Position (x+1) y East
moveForward (Position x y South) = Position x (y-1) South
moveForward (Position x y West) = Position (x-1) y West

startsWith :: String -> String -> Bool
startsWith "" _ = True
startsWith (x:xs) (y:ys) = if x == y then startsWith xs ys else False

convertStringToInstruction :: String -> Instruction
convertStringToInstruction "RIGHT" = TurnRight
convertStringToInstruction "LEFT" = TurnLeft
convertStringToInstruction "MOVE" = Move
convertStringToInstruction "REPORT" = Report
convertStringToInstruction other = if startsWith "PLACE" other then Place $ convertPosition other else Invalid

remove :: String -> Int -> String
remove string 0 = string
remove (x:xs) index = remove xs (index-1)

convertPosition :: String -> Position
convertPosition string = convert $ splitString (remove string 6) ',' ""

convert :: [String] -> Position
convert (x:y:direction:other) = Position (read x :: Int) (read y :: Int) (convertStringToDirection direction)

convertStringToDirection :: String -> Direction
convertStringToDirection "NORTH" = North
convertStringToDirection "EAST" = East
convertStringToDirection "SOUTH" = South
convertStringToDirection "WEST" = West

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

perform :: Maybe Position -> [Instruction] -> Maybe Position
perform position [] = position
perform Nothing (Place position:xs) = perform (Just position) xs
perform Nothing (_:xs) = perform Nothing xs
perform (Just position) (x:xs) = perform (Just $ performInstruction position x) xs

performInstruction :: Position -> Instruction -> Position
performInstruction _ (Place position) = position
performInstruction position Move = makeValidMove position
performInstruction (Position x y direction) TurnRight = Position x y $ turnRight direction
performInstruction (Position x y direction) TurnLeft = Position x y $ turnLeft direction