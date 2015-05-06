
import Data.List.Split
import Data.List
import Test.HUnit

getDigitChars :: String -> [DigitChars]
getDigitChars s = let lines = splitOn "\n" s in 
  let triplets = map (chunksOf 3) lines in
  map DigitChars (transpose triplets)

data DigitChars = DigitChars [String] deriving (Show, Eq)  

data Segments = Segments [Bool] deriving (Show, Eq)

getSegment :: DigitChars -> Segments
getSegment (DigitChars d) = (Segments . map (/= ' ') . concat) d


allSegs = map getSegment . getDigitChars $ "\
\ _     _  _     _  _  _  _  _ \n\
\| |  | _| _||_||_ |_   ||_||_|\n\
\|_|  ||_  _|  | _||_|  ||_| _|\n"


segToChar :: Segments -> Maybe Char
segToChar s = fmap head (fmap show (elemIndex s allSegs))

handleMaybeChar :: Maybe Char -> Char
handleMaybeChar (Just c) = c
handleMaybeChar Nothing  = '?'

parseLine :: String -> [Char]
parseLine s = (map (handleMaybeChar . segToChar) . map getSegment . getDigitChars) s

allZeros = "\
\ _  _  _  _  _  _  _  _  _ \n\
\| || || || || || || || || |\n\
\|_||_||_||_||_||_||_||_||_|\n\
\"

testAllZeros = TestCase (assertEqual "Parse failed" (parseLine allZeros) "000000000")

allOnes = "\
\                           \n\
\  |  |  |  |  |  |  |  |  |\n\
\  |  |  |  |  |  |  |  |  |\n\
\"

testAllOnes = TestCase (assertEqual "Parse failed" (parseLine allOnes) "111111111")

allTwos ="\
\ _  _  _  _  _  _  _  _  _ \n\
\ _| _| _| _| _| _| _| _| _|\n\
\|_ |_ |_ |_ |_ |_ |_ |_ |_ \n\
\"                           

testAllTwos = TestCase (assertEqual "Parse failed" (parseLine allTwos) "222222222")

allThrees ="\
\ _  _  _  _  _  _  _  _  _ \n\
\ _| _| _| _| _| _| _| _| _|\n\
\ _| _| _| _| _| _| _| _| _|\n\
\"

testAllThrees = TestCase (assertEqual "Parse failed" (parseLine allThrees) "333333333")
                           
allFours ="\
\                           \n\
\|_||_||_||_||_||_||_||_||_|\n\
\  |  |  |  |  |  |  |  |  |\n\
\"                           

testAllFours = TestCase (assertEqual "Parse failed" (parseLine allFours) "444444444")

allFives ="\
\ _  _  _  _  _  _  _  _  _ \n\
\|_ |_ |_ |_ |_ |_ |_ |_ |_ \n\
\ _| _| _| _| _| _| _| _| _|\n\
\"                           
testAllFives = TestCase (assertEqual "Parse failed" (parseLine allFives) "555555555")

allSixes ="\
\ _  _  _  _  _  _  _  _  _ \n\
\|_ |_ |_ |_ |_ |_ |_ |_ |_ \n\
\|_||_||_||_||_||_||_||_||_|\n\
\"                           
testAllSixes = TestCase (assertEqual "Parse failed" (parseLine allSixes) "666666666")

allSevens ="\
\ _  _  _  _  _  _  _  _  _ \n\
\  |  |  |  |  |  |  |  |  |\n\
\  |  |  |  |  |  |  |  |  |\n\
\"                           
testAllSevens = TestCase (assertEqual "Parse failed" (parseLine allSevens) "777777777")

allEights ="\
\ _  _  _  _  _  _  _  _  _ \n\
\|_||_||_||_||_||_||_||_||_|\n\
\|_||_||_||_||_||_||_||_||_|\n\
\"                           
testAllEights = TestCase (assertEqual "Parse failed" (parseLine allEights) "888888888")

allNines ="\
\ _  _  _  _  _  _  _  _  _ \n\
\|_||_||_||_||_||_||_||_||_|\n\
\ _| _| _| _| _| _| _| _| _|\n\
\"                           
testAllNines = TestCase (assertEqual "Parse failed" (parseLine allNines) "999999999")

allNums ="\
\    _  _     _  _  _  _  _ \n\
\  | _| _||_||_ |_   ||_||_|\n\
\  ||_  _|  | _||_|  ||_| _|\n\
\"                           
testAllNums = TestCase (assertEqual "Parse failed" (parseLine allNums) "123456789")

endsWithFive ="\
\ _  _  _  _  _  _  _  _    \n\
\| || || || || || || ||_   |\n\
\|_||_||_||_||_||_||_| _|  |\n\
\"
testEndsWithFives = TestCase (assertEqual "Parse failed" (parseLine endsWithFive) "000000051")

tests = TestList [TestLabel "Test all the zeros" testAllZeros, 
		  TestLabel "Test all the ones" testAllOnes, 
		  TestLabel "Test all the twos" testAllTwos, 
		  TestLabel "Test all the threes" testAllThrees, 
		  TestLabel "Test all the fours" testAllFours, 
		  TestLabel "Test all the fives" testAllOnes, 
		  TestLabel "Test all the sixes" testAllOnes, 
		  TestLabel "Test all the sevens" testAllOnes, 
		  TestLabel "Test all the eights" testAllOnes, 
		  TestLabel "Test all the nines" testAllOnes, 
		  TestLabel "Test all the nums" testAllOnes, 
		  TestLabel "Test the other one" testAllOnes
		  ]
