import Test.Hspec
import Test.QuickCheck
import System.FilePath
import InputHandler
import RandGrammar

parseTest path =
    it ("Parsing " ++ takeFileName path ++ "...") $ emit [path]

main :: IO ()
main = hspec $
    describe "Jack Parsing" $ do
        parseTest "../../11/Pong/Ball.jack"
        parseTest "../../11/Pong/Bat.jack"
        parseTest "../../11/Pong/Main.jack"
        parseTest "../../11/Pong/PongGame.jack"
        parseTest "../../11/Square/Main.jack"
        parseTest "../../11/Square/Square.jack"
        parseTest "../../11/Square/SquareGame.jack"
        parseTest "../../11/ConvertToBin/Main.jack"
        parseTest "../../11/ComplexArrays/Main.jack"
        --it "x + 1 is always greater than x" $
        --    property $ \x -> x + 1 > (x :: Int)
        --it "fail keyword test!" $
        --    property $ \(G.Keyword s) -> s == "this"
        --it "fail class var type test!" $
        --    property $ \ty -> ty == G.Field
