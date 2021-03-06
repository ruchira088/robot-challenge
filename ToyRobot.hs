module ToyRobot where

import Utils
import Errors

width = 5
height = 5

data Instruction = TurnRight | TurnLeft | Move | Report | Place Position deriving (Eq, Show)
data Direction = North | East | South | West deriving (Eq, Show)
data Position = Position Int Int Direction deriving (Eq, Show)

isValidPosition :: Position -> Bool
isValidPosition (Position x y _) = x >= 0 && x < width && y >= 0 && y < height

makeValidMove :: Position -> Position
makeValidMove position
  | isValidPosition $ moveForward position = moveForward position
  | otherwise                              = position

moveForward :: Position -> Position
moveForward (Position x y North) = Position x (y+1) North
moveForward (Position x y East) = Position (x+1) y East
moveForward (Position x y South) = Position x (y-1) South
moveForward (Position x y West) = Position (x-1) y West

startsWith :: String -> String -> Bool
startsWith "" _ = True
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

convertStringToInstruction :: String -> Instruction
convertStringToInstruction "RIGHT" = TurnRight
convertStringToInstruction "LEFT" = TurnLeft
convertStringToInstruction "MOVE" = Move
convertStringToInstruction "REPORT" = Report
convertStringToInstruction string
  | startsWith "PLACE" string = Place $ convertStringToPosition $ trimString $ remove string $ length "PLACE"
  | otherwise                 = invalidInstructionError

trimString :: String -> String
trimString string = reverse $ trimStringStart $ reverse $ trimStringStart string

trimStringStart :: String -> String
trimStringStart (' ':xs) = trimStringStart xs
trimStringStart str = str

remove :: String -> Int -> String
remove string 0 = string
remove (_:xs) index = remove xs (index-1)

convertStringToPosition :: String -> Position
convertStringToPosition string = convertToPosition $ splitString (trimString string) ',' ""

convertToPosition :: [String] -> Position
convertToPosition (x:y:direction:_) = Position (read x :: Int) (read y :: Int) (convertStringToDirection direction)
convertToPosition _ = invalidPositionError

convertStringToDirection :: String -> Direction
convertStringToDirection "NORTH" = North
convertStringToDirection "EAST" = East
convertStringToDirection "SOUTH" = South
convertStringToDirection "WEST" = West
convertStringToDirection _ = invalidDirectionError

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

performInstructions :: Maybe Position -> [Instruction] -> [Position] -> (Maybe Position, [Position])
performInstructions position [] output = (position, output)
performInstructions currentPosition (Place position:xs) output = performInstructions (if isValidPosition position then Just position else currentPosition) xs output
performInstructions Nothing (_:xs) output = performInstructions Nothing xs output
performInstructions (Just position) (Report:xs) output = performInstructions (Just position) xs (output ++ [position])
performInstructions (Just position) (x:xs) output = performInstructions (Just $ performInstruction position x) xs output

performInstruction :: Position -> Instruction -> Position
performInstruction _ (Place position) = position
performInstruction position Move = makeValidMove position
performInstruction (Position x y direction) TurnRight = Position x y $ turnRight direction
performInstruction (Position x y direction) TurnLeft = Position x y $ turnLeft direction

printPosition :: Position -> String
printPosition (Position x y direction) = show x ++ "," ++ show y ++ "," ++ show direction

describeOutput :: [Position] -> String
describeOutput [] = ""
describeOutput [x] = printPosition x
describeOutput (x:xs) = printPosition x ++ " " ++ describeOutput xs

getOutput :: (Maybe Position, [Position]) -> String
getOutput (_, reports) = describeOutput reports