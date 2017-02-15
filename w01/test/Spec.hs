import Test.HUnit
import Spring13Homework1
import Spring13Homework2

-- Credit Card
test1 = TestCase (assertEqual "for (toDigits 0)" [] (toDigits 0))
test2 = TestCase (assertEqual "for (toDigits -1)" [] (toDigits (-1)))
test3 = TestCase (assertEqual "for (toDigits 1)" [1] (toDigits 1))
test4 = TestCase (assertEqual "for (toDigits 1234)" [1,2,3,4] (toDigits 1234))

test5 = TestCase (assertEqual "for (toDigitsRev 0)" [] (toDigitsRev 0))
test6 = TestCase (assertEqual "for (toDigitsRev -1)" [] (toDigitsRev (-1)))
test7 = TestCase (assertEqual "for (toDigitsRev 1)" [1] (toDigitsRev 1))
test8 = TestCase (assertEqual "for (toDigitsRev 1234)" [4,3,2,1] (toDigitsRev 1234))

test9 = TestCase (assertEqual "for (doubleEveryOther [])" [] (doubleEveryOther []))
test10 = TestCase (assertEqual "for (doubleEveryOther [1])" [1] (doubleEveryOther [1]))
test11 = TestCase (assertEqual "for (doubleEveryOther [1,10])" [2,10] (doubleEveryOther [1,10]))
test12 = TestCase (assertEqual "for (doubleEveryOther [3,5,7,9,11])" [3,10,7,18,11] (doubleEveryOther [3,5,7,9,11]))

test13 = TestCase (assertEqual "for (sumDigits [])" 0 (sumDigits []))
test14 = TestCase (assertEqual "for (sumDigits [4])" 4 (sumDigits [4]))
test15 = TestCase (assertEqual "for (sumDigits [16])" 7 (sumDigits [16]))
test16 = TestCase (assertEqual "for (sumDigits [16,7,12,5])" 22 (sumDigits [16,7,12,5]))

test17 = TestCase (assertEqual "for (validate 4012888888881881)" True (validate 4012888888881881))
test18 = TestCase (assertEqual "for (validate 4012888888881882)" False (validate 4012888888881882))

tests1 = [
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4,
    TestLabel "test5" test5,
    TestLabel "test6" test6,
    TestLabel "test7" test7,
    TestLabel "test8" test8,
    TestLabel "test9" test9,
    TestLabel "test10" test10,
    TestLabel "test11" test11,
    TestLabel "test12" test12,
    TestLabel "test13" test13,
    TestLabel "test14" test14,
    TestLabel "test15" test15,
    TestLabel "test16" test16,
    TestLabel "test17" test17,
    TestLabel "test18" test18]

-- Towers of Hanoi
hTest1 = TestCase (assertEqual "for (hanoi -1 \"a\" \"b\" \"c\")" [] (hanoi (-1) "a" "b" "c"))
hTest2 = TestCase (assertEqual "for (hanoi 0 \"a\" \"b\" \"c\")" [] (hanoi 0 "a" "b" "c"))
hTest3 = TestCase (assertEqual "for (hanoi 1 \"a\" \"b\" \"c\")" [("a", "b")] (hanoi 1 "a" "b" "c"))
hTest4 = TestCase (assertEqual "for (hanoi 2 \"a\" \"b\" \"c\")" [("a", "c"), ("a", "b"), ("c", "b")] (hanoi 2 "a" "b" "c"))

tests2 = [
    TestLabel "hTest1" hTest1,
    TestLabel "hTest2" hTest2,
    TestLabel "hTest3" hTest3,
    TestLabel "hTest4" hTest4]

-- Towers of Hanoi 4
h4Test1 = TestCase (assertEqual "for (hanoi -1 \"a\" \"b\" \"c\" \"d\")" [] (hanoi4 (-1) "a" "b" "c" "d"))
h4Test2 = TestCase (assertEqual "for (hanoi 0 \"a\" \"b\" \"c\" \"d\")" [] (hanoi4 0 "a" "b" "c" "d"))
h4Test3 = TestCase (assertEqual "for (hanoi 1 \"a\" \"b\" \"c\" \"d\")" [("a", "b")] (hanoi4 1 "a" "b" "c" "d"))
h4Test4 = TestCase (assertEqual "for (hanoi 2 \"a\" \"b\" \"c\" \"d\")" [("a", "c"), ("a", "b"), ("c", "b")] (hanoi4 2 "a" "b" "c" "d"))
h4Test5 = TestCase (assertEqual "for (hanoi 3 \"a\" \"b\" \"c\" \"d\")" [("a", "c"), ("a", "d"), ("a", "b"), ("d", "b"), ("c", "b")] (hanoi4 3 "a" "b" "c" "d"))

tests3 = [
    TestLabel "h4Test1" h4Test1,
    TestLabel "h4Test2" h4Test2,
    TestLabel "h4Test3" h4Test3,
    TestLabel "h4Test4" h4Test4,
    TestLabel "h4Test5" h4Test5]

-- Hanoi display
hdTest1 = TestCase (assertEqual "for (startPositions 0 [])" [] (startPositions 0 []))
hdTest2 = TestCase (assertEqual "for (startPositions 1 [])" [("a", [0]), ("b", [])] (startPositions 1 ["a", "b"]))
hdTest3 = TestCase (assertEqual "for (startPositions 10 [])" [("a", [0..9]), ("b", [])] (startPositions 10 ["a", "b"]))

hdTest4 = TestCase (assertEqual "for (performMoves 10 [])" [("a", [0..9]), ("b", [])] (performMoves 10 [("a", "b"), ("b", "a")]))
hdTest5 = TestCase (assertEqual "for (performMoves 10 [])" [("a", [1, 2]), ("b", [0])] (performMoves 3 [("a", "b")]))

hdTest6 = TestCase (assertEqual "for (largestDisk [])" "" (boardToStr []))
hdTest7 = TestCase (assertEqual "for (largestDisk [(a,[0,1,2])])" 2 (largestDisk [("a", [0..2])]))

hdTest8 = TestCase (assertEqual "for (boardToStr [])" "" (boardToStr []))
hdTest9 = TestCase (assertEqual "for (boardToStr [(\"a\", [])])" " X " (boardToStr [("a", [])]))
hdTest10 = TestCase (assertEqual "for (boardToStr [(\"a\", [0])])" " - \n X " (boardToStr [("a", [0])]))
hdTest11 = TestCase (assertEqual "for (boardToStr [(\"a\", [0,1])])" "  -  \n --- \n XXX " (boardToStr [("a", [0,1])]))
hdTest12 = TestCase (assertEqual "for (boardToStr [(\"a\", [0]), (a, [1])])" "  |    |  \n  -   --- \n XXX  XXX " (boardToStr [("a", [0]), ("b", [1])]))

tests4 = [
    TestLabel "hdTest1" hdTest1,
    TestLabel "hdTest2" hdTest2,
    TestLabel "hdTest3" hdTest3,
    TestLabel "hdTest4" hdTest4,
    TestLabel "hdTest5" hdTest5,
    TestLabel "hdTest6" hdTest6,
    TestLabel "hdTest7" hdTest7,
    TestLabel "hdTest8" hdTest8,
    TestLabel "hdTest9" hdTest9,
    TestLabel "hdTest10" hdTest10,
    TestLabel "hdTest11" hdTest11,
    TestLabel "hdTest12" hdTest12]

main :: IO Counts
main = runTestTT (TestList (tests1 ++ tests2 ++ tests3 ++ tests4))