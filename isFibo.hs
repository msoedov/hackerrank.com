--Input Format	
--
--First line contains T, number of test cases.
--T lines follows. Each line will contain an integer N.
--
--Output Format
--
--Output “IsFibo” (without quotes) if N is a fibonacci 
--number and “IsNotFibo” (without quotes) if it is 
--not a fibonacci number, in a new line corresponding to each test case.

import Control.Monad(when, replicateM) 

sq :: Int -> Int
sq = (^2).floor . sqrt . fromIntegral

perfectSqrt :: Int -> Bool
perfectSqrt x
			|x == sq x = True
			|otherwise = False 

isFibo :: Int -> String
isFibo x 
		|(perfectSqrt $ 5 * x ^2 + 4) || (perfectSqrt $ 5 * x ^2 - 4) = "IsFibo"
		|otherwise = "IsNotFibo"

trait = do
		l <- getLine
		--when (not $ null l) $do 
		putStrLn $ isFibo $ read l

main = do
	t <- getLine
	replicateM (read t) trait