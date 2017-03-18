import Utils
import ToyRobot

main = do
    contents <- readFile "input.txt"
    print $ "Output: " ++ (getOutput $ performInstructions Nothing (map convertStringToInstruction $ splitLines contents) [])