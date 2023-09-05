--------------------------------------------------------------------------
--                                                                      --
--	Section 6.1: Functions as arguments				--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Mapping a numeric function along a list of numbers.		--
-------------------------------------------------------------------------- 

mapInt :: (Int -> Int) -> [Int] -> [Int]

mapInt f []    = []
mapInt f (a:x) = f a : mapInt f x

-------------------------------------------------------------------------- 
--	Double or treble every element of a list of numbers.		--
-------------------------------------------------------------------------- 

double l = mapInt times2 l
treble l = mapInt times3 l

times2,times3 :: Int -> Int

times2 n = 2*n
times3 n = 3*n

exam1 = mapInt times2 [2,3]

-------------------------------------------------------------------------- 
--	Alternative definition using a list comprehension.		--
-------------------------------------------------------------------------- 

mapInt' f l = [ f a | a <-l ]

-------------------------------------------------------------------------- 
--	A function to give the sum f 0 + ... + f n.			--
-------------------------------------------------------------------------- 

total :: (Int -> Int) -> Int -> Int

total f n
  | n==0 	= f 0
  | otherwise 	= total f (n-1) + f n

-------------------------------------------------------------------------- 
--	The maximum of the values f 0 , ..., f n.			--
-------------------------------------------------------------------------- 

maxFun :: (Int -> Int) -> Int -> Int

maxFun f n
  | n==0 	= f 0
  | otherwise 	= max (maxFun f (n-1)) (f n)

-------------------------------------------------------------------------- 
--	Is one of f 0 , ..., f n zero?					--
-------------------------------------------------------------------------- 

zeroInRange :: (Int -> Int) -> Int -> Bool

zeroInRange f 0 = (f 0 == 0)
zeroInRange f n = zeroInRange f (n-1) || (f n == 0)

-------------------------------------------------------------------------- 
--	Folding a numerical operator into a list of numbers.		--
-------------------------------------------------------------------------- 

foldInt :: (Int -> Int -> Int) -> [Int] -> Int

foldInt f [a]     = a
foldInt f (a:b:x) = f a (foldInt f (b:x))

-------------------------------------------------------------------------- 
--	Using foldInt.							--
-------------------------------------------------------------------------- 

sumList l = foldInt (+) l
maxList l = foldInt max l

-------------------------------------------------------------------------- 
--	Properties of characters.					--
--									--
-- 	isDigit,isAlpha are predefined functions of type		--
--	Char -> Bool							--
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Filtering a string for the characters with a given		--
--	property.							--
-------------------------------------------------------------------------- 

filterString :: (Char -> Bool) -> [Char] -> [Char]

filterString p [] = []
filterString p (a:x)
  | p a 	= a : filterString p x
  | otherwise 	=     filterString p x

-------------------------------------------------------------------------- 
--	Using filterString						--
--	In Haskell 1.3 and Hugs0 need to add the definition of isDigit	--
--	given in Section 4.3.						--
--	Also isAlpha needs to be defined -- exercise for the reader.	--
-------------------------------------------------------------------------- 

digits  st = filterString isDigit st
letters st = filterString isAlpha st

-------------------------------------------------------------------------- 
--	Defining filterString using a list comprehension.		--
-------------------------------------------------------------------------- 

filterString' p x = [ a | a<-x , p a ]

