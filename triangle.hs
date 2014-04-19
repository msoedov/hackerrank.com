--For a given integer K, print the first K rows of Pascal’s Triangle. 
--Print each row with values separated by spaces. The value at nthrow and 
--rth column of the triangle is equal to n! / (r! * (n-r)!) where indexing start from 0. 
--These values are the binomial coefficients. 
import Data.Char
import Data.List

coupling :: [Int] -> [Int]
coupling [a, b] = [a + b]
coupling (x:y:xs) = (x+y):coupling (y:xs)

makeLine :: [Int] -> [Int]
makeLine [1] = [1, 1]
makeLine lst = 1:(coupling lst ) ++ [1]

putList :: [[Int]] -> [String]
putList [] = []
putList (x:xs) = (intercalate " " $ map show x) : putList xs


main :: IO ()
main = do
	n <- getLine
	sequence_ $ map putStrLn $ putList $ take (read n::Int) (iterate makeLine [1])