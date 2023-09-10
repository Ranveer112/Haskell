
module Chap7 where
import Prelude hiding(zip3, splitAt, drop, unzip, reverse, and, or, product)

firstElementIncrement :: [Int] -> Int
firstElementIncrement [] = 0
firstElementIncrement (x:_) = x


sumOfFirstTwo :: [Int] -> Int
sumOfFirstTwo [] = 0
sumOfFirstTwo (x:[]) = x
sumOfFirstTwo (x:xs) = x + sumOfFirstTwo xs


product :: [Int] -> Int
product [] = 1
product (x:xs) = x * product xs


and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs


or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

elemNum :: Int -> [Int] -> Int
elemNum x [] = 0
elemNum x (f:fs) | x == f = 1 + elemNum x fs
		 | otherwise = elemNum x fs


unique :: [Int] -> [Int]
unique [] = []
unique (x:[]) = [x]
unique (x:xs) | elemNum x xs >= 1 = unique xs
	      | otherwise = (x:unique xs)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]


unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip (f:fs) = ([a] ++ as, [b] ++ bs)
		 where
		 (as, bs) = unzip fs
		 (a, b) = f

drop :: Int -> [a] -> [a] 
drop 0 x  = x
drop n (x:xs) | n > 0 = (drop (n-1) xs)
drop n _  | n < 0 = error "PreludeList.drop negative argument"
	  | otherwise = []


splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 x = ([], x)
splitAt n (x:xs) | n > 0 = ([x] ++ y, [] ++ ys) 
		           where
	                   (y, ys) = (splitAt (n-1) xs)
splitAt n _ | n < 0 = error "PreludeList.splitAt negative argument"
            | otherwise = ([], [])


zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 (a:as) (b:bs) (c:cs) = [(a, b, c)] ++ zip3 as bs cs
zip3 _ _ _ = []

-- Using zip
_zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
_zip3 x y z = [(a, b1, c) | ((a, b), (b1, c)) <- zipped] 
	       where
	       zipped = (zip (zip x y) (zip y z)) 

isSubList :: [Char] -> [Char] -> Bool
isSubList pattern [] = pattern == []
isSubList pattern search_space =  latterSearchRes || pattern == (take (length pattern) search_space)
				  where
				  f:fs = search_space
				  latterSearchRes = isSubList pattern fs 



isSubSequence :: [Char] -> [Char] -> Bool
isSubSequence pattern [] = pattern == []
isSubSequence [] search_space = True
isSubSequence pattern search_space | head pattern == head search_space = isSubSequence (tail pattern) (tail search_space)
			           | otherwise = isSubSequence pattern (tail search_space)
