module Utils where

splitLines :: String -> [String]
splitLines input = splitString input '\n' ""

splitString :: String -> Char -> String -> [String]
splitString "" _ word = [word]
splitString (x:xs) delimiter word = if (x == delimiter) then [word] ++ splitString xs delimiter "" else splitString xs delimiter (word ++ [x])
