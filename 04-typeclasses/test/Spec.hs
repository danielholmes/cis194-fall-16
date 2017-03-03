import Test.Hspec
import Control.Exception
import Exercise1

main :: IO ()
main = hspec $ do
    describe "elemList" $ do
        it "returns false for empty" $
            elemList 1 Empty `shouldBe` False

        it "returns true for present in list" $
            elemList 1 (Entry 2 (Entry 1 Empty)) `shouldBe` True

        it "returns false for not present in list" $
            elemList 1 (Entry 2 (Entry 3 Empty)) `shouldBe` False

    describe "listLength" $ do
        it "returns 0 for empty" $
            listLength Empty `shouldBe` 0

        it "returns 1 for entry" $
            listLength (Entry 1 Empty) `shouldBe` 1

        it "returns 2 for multiple entry" $
            listLength (Entry 1 (Entry 2 Empty)) `shouldBe` 2

    describe "appendList" $ do
        it "returns empty for 2 empties" $
            let result = appendList Empty Empty
            in listLength result `shouldBe` 0

        it "returns first for second empty" $
            appendList (Entry 1 Empty) Empty `shouldBe` (Entry 1 Empty)

        it "returns second for first empty" $
            appendList Empty (Entry 1 Empty) `shouldBe` (Entry 1 Empty)

    describe "filterList" $ do
        it "returns empty for empty" $
            let result = filterList id Empty
            in listLength result `shouldBe` 0

        it "returns full list for matching pred" $
            filterList (const True) (Entry 1 (Entry 2 Empty)) `shouldBe` (Entry 1 (Entry 2 Empty))

        it "returns empty for not matching pred" $
            filterList (const False) (Entry 1 (Entry 2 Empty)) `shouldBe` Empty

    describe "nth" $ do
        it "throws error for empty" $
            evaluate (nth Empty 1) `shouldThrow` errorCall "list too short"

        it "returns first element for 1" $
            nth (Entry 'a' Empty) 1 `shouldBe` 'a'

        it "returns last element" $
            nth (Entry 'a' . Entry 'b' . Entry 'c' $ Empty) 3 `shouldBe` 'c'