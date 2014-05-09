import System.IO
import Data.List

merge :: Ord a => [a] -> [a] -> [a]
merge [] nonempty = nonempty
merge left@(l:lt) right@(r:rt) = if l < r then l:(merge lt right ) 
								 else  r:(merge rt left)


splitList :: Ord a => [a] -> ([a], [a])
splitList [] = ([], [])
splitList [single] = ([single], [])
splitList (f:s:tails) = let (fList, sList) = splitList tails
						in ( f:fList, s:sList)

depthSort :: Ord a => [a] -> [a]
depthSort [val] = [val]
depthSort [g, l] = if g > l then [l, g] else [g, l]
depthSort lst = let (f, s) = splitList lst
				in merge (depthSort f) (depthSort s)


inverseF :: Ord a => [a] -> a -> [a]
inverseF = undefined


main = do
    handle <- openFile "100kIntegers.txt" ReadMode  
    contents <- hGetContents handle
    hClose handle
    print $ depthSort $ lines contents
