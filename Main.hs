import Utils
import ToyRobot

main = do
    contents <- readFile "input.txt"
    print $ "Output: " ++ (getOutput $ perform Nothing (map convertStringToInstruction $ splitLines contents) [])