import Test.Hspec
import Control.Exception
import List
import Exercise1
import Exercise2
import Exercise3
import Mazes

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

    describe "isGraphClosed" $ do
        it "returns false for always false" $
            isGraphClosed 1 (const Empty) (const False) `shouldBe` False

        it "returns true for initial true" $
            isGraphClosed 1 (const Empty) (==1) `shouldBe` True

        it "returns true for all true" $
            isGraphClosed 3 (const (Entry 3 (Entry 3 Empty))) (==3) `shouldBe` True

        it "returns true for adjacent false" $
            isGraphClosed 1 (const (Entry 2 (Entry 3 Empty))) (==1) `shouldBe` False

    describe "allAdjacent" $ do
        it "returns correct" $
            allAdjacent (C 0 0) `shouldBe` (Entry (C 0 1) (Entry (C 0 (-1)) (Entry (C (-1) 0) (Entry (C 1 0) Empty))))

    describe "adjacentNonWalls" $ do
        it "returns correct for no walls" $
            adjacentNonWalls (const Wall) (C 0 0) `shouldBe` Empty

    describe "isWalkableCoord" $ do
        it "returns false for blank" $
            isWalkableCoord (tileAtOriginOtherwise Storage Blank) (C 5 5) `shouldBe` False

        it "returns true for storage" $
            isWalkableCoord (tileAtOriginOtherwise Storage Storage) (C 5 5) `shouldBe` True

    describe "isClosed" $ do
        it "returns false for initial being a box" $
            isClosed (Maze (C 0 0) (tileAtOriginOtherwiseWall Box)) `shouldBe` False

        it "returns true for initial being a storage" $
            isClosed (Maze (C 0 0) (tileAtOriginOtherwiseWall Storage)) `shouldBe` True

        it "returns true for initial being a ground" $
            isClosed (Maze (C 0 0) (tileAtOriginOtherwiseWall Ground)) `shouldBe` True

        it "returns true for initial being a ground" $
            isClosed (Maze (C 0 0) (tileAtOriginOtherwiseWall Ground)) `shouldBe` True

        it "returns true for maze3" $
            isClosed (Maze (C (-4) 3) maze3) `shouldBe` True

        it "returns false for blank" $
            isClosed (Maze (C 0 0) (tileAtOriginOtherwise Storage Blank)) `shouldBe` False
