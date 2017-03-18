
# Seek Coding Test
Toy robot simulator

## Getting Started
The inputs are passed via the `input.txt` file in the root project folder.

##### Example

```text
PLACE 1,2,EAST
MOVE
MOVE
LEFT
MOVE
REPORT
```

To run the application, navigate to the root project directory and type

```bash
runhaskell Main.hs
```

This will print the output on the command line

##### Example

```bash
Output: 3,3,North
```

## Testing
Make sure that you have [cabal](https://www.haskell.org/cabal/download.html) installed on your computer.

Fetch the necessary dependencies

```bash
cabal update
cabal install hspec
```

Execute the test suite by navigating to the project root directory and typing

```bash
runhaskell Test.hs
```