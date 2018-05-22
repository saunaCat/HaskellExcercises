import Data.List.Split
import Data.Char

-- problem 1

removeMultiples:: [Int] -> [Int]
removeMultiples xs = filter findMultiples xs

findMultiples:: Int -> Bool
findMultiples x 
  | mod x 35 == 0 = True
  | mod x 7 == 0  = False
  | mod x 5 == 0  = False
  | otherwise     = True
      
-- problem 2

dollars:: String -> String
dollars "" = "zero dollars and zero cents"
dollars s  = do
               let (dollars, cents) = splitDollarsAndCents s
               concat [numToText dollars,  " dollars and ", numToText cents, " cents"]
               

splitDollarsAndCents:: String -> (Int, Int)
splitDollarsAndCents "" = (0, 0)
splitDollarsAndCents s = do 
                           let [dollars, cents] = splitOn "." s
                           (read dollars:: Int, read cents:: Int)

numToText:: Int -> String
numToText num = case num of 1  -> "one"
                            2  -> "two"
                            3  -> "three"
                            4  -> "four"
                            5  -> "five"
                            6  -> "six"
                            7  -> "seven"
                            8  -> "eight"
                            9  -> "nine"
                            10 -> "ten"
                            -- reduce ....

















