--------------------------------------------------------------------------
--                                                                      --
--	Section 10.5: Design with Algebraic Data Types			--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	To use this in Haskell 1.3, the occurrences of the class Text	--
--	need to be replaced by Read, Show or both these classes.	--
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Edit distance							--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	The type of editing operations.					--
-------------------------------------------------------------------------- 

data Edit = Change Char |
            Copy |
            Delete |
            Insert Char |
            Kill  
	    deriving (Eq,Text)

notCopy :: Edit -> Bool

notCopy Copy = False
notCopy _    = True

-------------------------------------------------------------------------- 
--	Transforming a string into another -- gives a sequence of edits	--
-------------------------------------------------------------------------- 

transform :: String -> String -> [Edit]

transform [] [] = []
transform st [] = [Kill]
transform [] st = map Insert st

transform (a:x) (b:y)
  | a==b 	= Copy : transform x y
  | otherwise 	= best [ Delete   : transform x (b:y) ,
                         Insert b : transform (a:x) y ,
                         Change b : transform x y ]

-------------------------------------------------------------------------- 
--	Finding the best (lowest cost) sequence of edits in a 		--
--	list of such.							--
-------------------------------------------------------------------------- 

best :: [[Edit]] -> [Edit]

best [a]   = a
best (a:x) 
  | cost a <= cost b 	= a
  | otherwise 		= b
      where 
      b = best x

-------------------------------------------------------------------------- 
--	The cost of a given sequence -- count one for all but the 	--
--	copy operation.							--
-------------------------------------------------------------------------- 

cost :: [Edit] -> Int

cost = length . filter (/=Copy)


-------------------------------------------------------------------------- 
--	Example: Simulation						--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Incoming messages.						--
-------------------------------------------------------------------------- 

data Inmess = No | Yes Arrival Service

type Arrival = Int
type Service = Int

-------------------------------------------------------------------------- 
--	Outgoing messages.						--
-------------------------------------------------------------------------- 

data Outmess = None | Discharge Arrival Wait Service

type Wait = Int

