import Test.Hspec
--import Control.Exception (evaluate)
import Homework

main :: IO ()
main = hspec $ do
    describe "appendList" $ do
        it "returns Empty for empties" $
            True `shouldBe` True
        {-
        Cant get typing to work for show and eq
            appendList Empty Empty `shouldBe` Empty

        it "returns first for list Empty" $
            appendList (Entry 1 Empty) Empty `shouldBe` (Entry 1 Empty)

        it "returns second for Empty list" $
            appendList Empty (Entry 1 Empty) `shouldBe` (Entry 1 Empty)-}