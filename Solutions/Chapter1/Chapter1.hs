-------------------------------------------------------------------------- 
--									--
--	Chapter 1: Introducing functional programming			--
--									--
--	(c) Simon Thompson, 1995.					--
--									--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	The first example program.					--
--									--
--	((n - 3) * 2) + 6						--
-------------------------------------------------------------------------- 


-------------------------------------------------------------------------- 
--	Definitions from the example script.				--
-------------------------------------------------------------------------- 

answer :: Int               --  A number constant 
answer = 42  

newline :: Char 
newline = 'n'

yes :: Bool                 --  The answer yes is represented  
yes = True                  --  by the Boolean value True.

greater :: Bool             --  Uses the value of answer
greater = (answer>71)

-------------------------------------------------------------------------- 
--	Examples of function definitions.				--
-------------------------------------------------------------------------- 

square :: Int -> Int
square x = x*x

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n==m) && (m==p)

maxi :: Int -> Int -> Int
maxi n m 
  | (n>=m) 	= n
  | otherwise 	= m

-------------------------------------------------------------------------- 
--	Expressions to evaluate.					--
--									--
--	(2+3)								--
--	$$								--
--	5-4-3								--
--	7/3								--
--	7 `div` 3							--
--	4 `div` $$							--
--	7 `mod` 3							--
--	True || False							--
--	 not False && False						--
--									--
--	answer + 42							--
--	greater								--
--	square answer							--
--	square $$							--
--	allEqual 2 3 3							--
--	allEqual 5 5 5							--
--	allEqual (square 5) answer (maxi (-4) 2)			--
--									--
--	2+(3+4								--
--	2+(3+4))							--
--	True + 4							--
--	4 && True							--
--	4 5								--
--	7 did 4								--
--	4 `div` (3*2-6)							--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Type specifications.						--
-------------------------------------------------------------------------- 

-- allFourEqual :: Int -> Int -> Int -> Int -> Bool

-- howManyEqual :: Int -> Int -> Int -> Int

-- howManyOfTwoEqual :: Int -> Int -> Int

cube :: Int -> Int
cube x = x * square x

