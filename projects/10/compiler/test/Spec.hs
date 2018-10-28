import Test.Hspec
import System.FilePath
import InputHandler

parseTest path =
    it ("Parsing " ++ takeFileName path ++ "...") $
            emit [path] >>= (\res -> res `shouldBe` [True])

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
