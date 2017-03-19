import Test.Hspec
import ToyRobot

main :: IO ()
main = hspec $ do
  describe "ToyRobot" $ do
    it "convertStringToInstruction" $ do
      convertStringToInstruction "RIGHT" `shouldBe` TurnRight
      convertStringToInstruction "LEFT" `shouldBe` TurnLeft
      convertStringToInstruction "MOVE" `shouldBe` Move
      convertStringToInstruction "REPORT" `shouldBe` Report
      convertStringToInstruction "PLACE 1,1,NORTH" `shouldBe` Place (Position 1 1 North)
      convertStringToInstruction "PLACE 0,3,SOUTH" `shouldBe` Place (Position 0 3 South)

    it "performInstruction" $ do
      performInstruction (Position 0 0 North) Move `shouldBe` Position 0 1 North
      performInstruction (Position 1 1 South) Move `shouldBe` Position 1 0 South
      performInstruction (Position 2 2 East) Move `shouldBe` Position 3 2 East
      performInstruction (Position 2 2 West) Move `shouldBe` Position 1 2 West
      performInstruction (Position 0 0 North) (Place (Position 1 1 West)) `shouldBe` Position 1 1 West
      performInstruction (Position 1 1 East) TurnRight `shouldBe` Position 1 1 South
      performInstruction (Position 0 0 West) TurnLeft `shouldBe` Position 0 0 South
    
    it "performInstructions" $ do
      performInstructions Nothing [] [] `shouldBe` (Nothing, [])
      performInstructions Nothing [Place (Position 1 1 North)] [] `shouldBe` (Just (Position 1 1 North), [])
      performInstructions Nothing [Move, Move, Report, Place (Position 2 2 South), Move] [] `shouldBe` (Just (Position 2 1 South), [])
      performInstructions Nothing [Place (Position 5 5 North), Move, Move] [] `shouldBe` (Nothing, []) 
      performInstructions Nothing [Place (Position 1 1 South), Move, Place (Position 10 10 North)] [] `shouldBe` (Just (Position 1 0 South), [])
      performInstructions (Just (Position 1 1 East)) [] [] `shouldBe` (Just(Position 1 1 East), [])
      performInstructions (Just (Position 0 1 South)) [Move, Move, Move] [] `shouldBe` (Just (Position 0 0 South), [])
      performInstructions (Just (Position 2 2 North)) [Report, Move, Move, Report] [] `shouldBe` (Just (Position 2 4 North), [Position 2 2 North, Position 2 4 North])
      performInstructions (Just (Position 0 1 East)) [Report, Move, Place (Position 5 5 North)] [] `shouldBe` (Just (Position 1 1 East), [Position 0 1 East])
      performInstructions (Just (Position 3 3 West)) [Move, Move, Report, Place (Position 4 4 East)] [] `shouldBe` (Just (Position 4 4 East), [Position 1 3 West])

    it "describeOutput" $ do
      describeOutput [] `shouldBe` ""
      describeOutput [Position 1 1 West] `shouldBe` "1,1,West"
      describeOutput [Position 0 0 North, Position 2 2 East] `shouldBe` "0,0,North 2,2,East"