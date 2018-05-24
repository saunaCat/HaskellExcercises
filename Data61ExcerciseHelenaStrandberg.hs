import Data.List.Split
import Data.Char

-- problem 1

removeMultiples:: [Int] -> [Int]
removeMultiples xs = filter findMultiples xs

findMultiples:: Int -> Bool
findMultiples x 
  | mod x 35 == 0  = True
  | mod x 7  == 0  = False
  | mod x 5  == 0  = False
  | otherwise      = True
      
-- problem 2

dollars:: String -> String
dollars "" = "zero dollars and zero cents"
dollars s  
  | isStringValid s == False = "Invalid string. You must input an int, float or double!"
  | otherwise                = concat [numToText dollars, " dollars and ", 
                                       numToText (cents), " cents"]
  where (dollars, cents) = splitIntoDollarsAndCents s
               
isStringValid:: String -> Bool
isStringValid "" = True
isStringValid s 
  | all isDigit [ x | x <- s, not (x `elem` ".") ] == False = False
  | otherwise                      = True


splitIntoDollarsAndCents:: String -> (Int, Int)
splitIntoDollarsAndCents "" = (0, 0)
splitIntoDollarsAndCents s = do 
                               let [wholeDollars, cents] = splitIntoList s
                               textToNum [wholeDollars, (concat ["0.",cents])]
                                
splitIntoList:: String -> [String]
splitIntoList "" = ["", ""]
splitIntoList s 
  | length xs == 1 = [ head xs , ""]
  | otherwise      = xs
    where xs = splitOn "." s
    

textToNum:: [String] -> (Int, Int)
textToNum [] = (0, 0)
textToNum [wholeDollars, cents] =
  (read  $ zeroIfEmpty wholeDollars:: Int, 
   round $ (read $ zeroIfEmpty cents:: Double)*100)
                           
zeroIfEmpty:: String -> String
zeroIfEmpty "" = "0"
zeroIfEmpty s = s

numToText:: Int -> String
numToText num = case num of 0 -> "zero"
                            1 -> "one"
                            2  -> "two"
                            3  -> "three"
                            4  -> "four"
                            5  -> "five"
                            6  -> "six"
                            7  -> "seven"
                            8  -> "eight"
                            9  -> "nine"
                            10 -> "ten"
                            11 -> "eleven"
                            20 -> "twenty"
                            -- reduce ....?
                            

-- map first number and size of the rest to convert int into number word.... iterate..
numeral:: (Int, Int) -> String
numeral (firstNumber, sizeOfEntireNumber) = ""

















