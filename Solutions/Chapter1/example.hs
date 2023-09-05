-------------------------------------------------------------------------- 
--									--
--	example.hs							--
--									--
--	Some example definitions to illustrate the form of Haskell	--
--	scripts								--
--									--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Definitions from the example script.				--
-------------------------------------------------------------------------- 

answer :: Int               --  An integer constant 
answer = 42  

newline :: Char 
newline = 'n'

yes :: Bool                 --  The answer yes is represented  
yes = True                  --  by the Boolean value True.

greater :: Bool             --  Uses the value of answer
greater = (answer>71)

-------------------------------------------------------------------------- 
--	To square a whole number					--
-------------------------------------------------------------------------- 

square :: Int -> Int
square x = x*x

--------------------------------------------------------------------------
--	Are three whole numbers equal?					--
--------------------------------------------------------------------------

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n==m) && (m==p)

--------------------------------------------------------------------------
--      The maximum of two integers					--
--------------------------------------------------------------------------

maxi :: Int -> Int -> Int
maxi n m 
  | (n>=m) 	= n
  | otherwise 	= m

