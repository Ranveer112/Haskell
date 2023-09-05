--------------------------------------------------------------------------
--                                                                      --
--	Section 6.4: Using the higher-order functions			--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Type definitions used.						--
-------------------------------------------------------------------------- 

type Book = String
type Person = String
type Database = [ (Person,Book) ]

-------------------------------------------------------------------------- 
--	Redefining database functions.					--
-------------------------------------------------------------------------- 

books :: Database -> Person -> [Book]

books db per = map snd (filter isPer db)
               where
               isPer (p,b) = (p == per)


returnLoan :: Database -> Person -> Book -> Database

returnLoan db p b 
  = filter notPB db
    where
    notPB pr = (pr /= (p,b))

-------------------------------------------------------------------------- 
--	Exercise function declarations.					--
-------------------------------------------------------------------------- 

--  filterFirst :: (t -> Bool) -> [t] -> [t]
--  filterLast  :: (t -> Bool) -> [t] -> [t]


