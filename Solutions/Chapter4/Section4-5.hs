--------------------------------------------------------------------------
--                                                                      --
--	Section 4.5: A library database					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Types used in the system.					--
-------------------------------------------------------------------------- 

type Person = String
type Book   = String

-------------------------------------------------------------------------- 
--	The database type.						--
-------------------------------------------------------------------------- 

type Database = [ (Person , Book) ] 

exampleBase 
  = [ ("Alice" , "Postman Pat") , ("Anna" , "All Alone") ,
      ("Alice" , "Spot") , ("Rory" , "Postman Pat") ]

-------------------------------------------------------------------------- 
--	Types of lookup functions.					--
-------------------------------------------------------------------------- 

--  books       :: Database -> Person -> [Book]
--  borrowers   :: Database -> Book -> [Person]
--  borrowed    :: Database -> Book -> Bool
--  numBorrowed :: Database -> Person -> Int

-------------------------------------------------------------------------- 
--	Types of update functions.					--
-------------------------------------------------------------------------- 

--  makeLoan   :: Database -> Person -> Book -> Database
--  returnLoan :: Database -> Person -> Book -> Database

-------------------------------------------------------------------------- 
--	Examples:							--
--									--
--	books exampleBase "Alice" = [ "Postman Pat" , "Spot" ]		--
--	books exampleBase "Rory"  = [ "Postman Pat" ]			--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Defining books; the definitions of borrowers, borrowed and	--
--	numBorrowed are an exercise.					--
-------------------------------------------------------------------------- 

books :: Database -> Person -> [Book]

books [] borrower = []

books ((pers,bk):rest) borrower
  | pers == borrower 	= bk : books rest borrower
  | otherwise 		=      books rest borrower

-------------------------------------------------------------------------- 
--	Defining the update functions					--
-------------------------------------------------------------------------- 

makeLoan   :: Database -> Person -> Book -> Database

makeLoan dBase pers bk = (pers,bk) : dBase

-------------------------------------------------------------------------- 
--	returnLoan only removes the first pair (pers,bk).		--
-------------------------------------------------------------------------- 

returnLoan :: Database -> Person -> Book -> Database

returnLoan ((p,b):rest) pers bk
  | (p==pers && b==bk) 	= rest
  | otherwise 		= (p,b) : returnLoan rest pers bk

returnLoan [] pers bk
  = error ("returnLoan failed on " ++ pers ++ " " ++ bk)

-------------------------------------------------------------------------- 
--	An alternative, *different*, definition of returning a loan.	--
--	All (pers,bk) pairs are removed.				--
-------------------------------------------------------------------------- 

returnLoan2 :: Database -> Person -> Book -> Database

returnLoan2 ((p,b):rest) pers bk
  | (p==pers && b==bk) 	=         returnLoan2 rest pers bk
  | otherwise 		= (p,b) : returnLoan2 rest pers bk

returnLoan2 [] pers bk = []

-------------------------------------------------------------------------- 
--	Testing								--
--									--
--  	test0 = makeLoan [] "Alice" "Fireman Sam"			--
--  	test1 = borrowed exampleBase "Spot"				--
--  	test2 = makeLoan exampleBase "Alice" "Fireman Sam"		--
-------------------------------------------------------------------------- 


