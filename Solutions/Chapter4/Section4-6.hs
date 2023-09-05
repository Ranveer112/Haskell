--------------------------------------------------------------------------
--                                                                      --
--	Section 4.6: List comprehensions				--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	A series of examples.						--
-------------------------------------------------------------------------- 

ex :: [Int]

ex = [2,4,7]

exam1 = [ 2*a | a<-ex]

exam2 = [ isEven n | n<-ex ] 

exam3 = [ 2*a | a<-ex , isEven a , a>3 ]

-------------------------------------------------------------------------- 
--	The isEven check.						--
-------------------------------------------------------------------------- 

isEven :: Int -> Bool

isEven n = ((n `mod` 2) == 0) 

-------------------------------------------------------------------------- 
--	Summing a list of pairs.					--
-------------------------------------------------------------------------- 

addPairs :: [(Int,Int)] -> [Int]

addPairs pairList = [ a+b | (a,b) <- pairList ]

-------------------------------------------------------------------------- 
--	A variant of the previous function.				--
-------------------------------------------------------------------------- 

newAddPairs :: [(Int,Int)] -> [Int]

newAddPairs pairList = [ a+b | (a,b) <- pairList , a<b ]


-------------------------------------------------------------------------- 
--	Double every element of a list.					--
-------------------------------------------------------------------------- 

double :: [Int] -> [Int]

double l = [ 2*a | a<-l ]

-------------------------------------------------------------------------- 
--	Picking out the digits in a string.				--
--	As for Section 4.3, a definition of isDigit needs to be		--
--	added for Hugs0.						--
-------------------------------------------------------------------------- 

digits :: String -> String

digits st = [ ch | ch<-st , isDigit ch ] 

-------------------------------------------------------------------------- 
--	Revising the database functions.				--
-------------------------------------------------------------------------- 

type Person = String
type Book   = String

type Database = [ (Person,Book) ]

books :: Database -> Person -> [Book]

books db borrower 
  = [ bk | (per,bk) <- db , per==borrower ]

-------------------------------------------------------------------------- 
--	An erroneous definition of books.				--
-------------------------------------------------------------------------- 

books' :: Database -> Person -> [Book]

books' db borrower 
  = [ bk | (borrower,bk) <- db ]


