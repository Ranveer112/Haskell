--------------------------------------------------------------------------
--                                                                      --
--	Section 14.1: Stream-based interactive programs			--
--	Haskell version							--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

import Prelude hiding (lookup,fail)
import ParsingBasics
import WhyInfLists

--------------------------------------------------------------------------
--	To load this file need pre-loaded:				--
--	../Chapter13/Section13-5.hs					--
--	../Chapter13/Section13-7.hs					--
--------------------------------------------------------------------------

infixr 9 >.>

f >.> g = g . f

--------------------------------------------------------------------------
--	These simple interactions are run using the function run.	--
--		run :: (String -> String) -> Dialogue			--
--	For instance 							--
--		run example1						--
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Input and output types.						--
-------------------------------------------------------------------------- 

type Input  = String
type Output = String

--------------------------------------------------------------------------
--	First examples							--
--------------------------------------------------------------------------

example1 :: Input -> Output

example1 = lines >.> map reverse >.> unlines

ex1 = lines "hello\nbard" 
ex2 = unlines ["hello","bard"] 

example2 :: Input -> Output

example2 = lines >.> map read >.> map (+1) >.> map show >.> unlines

example3 = lines >.> map reverse >.> map (++"\nAnother?") >.> unlines

example4 = example3 >.> ("Enter a string or Control-D\n"++) 

-------------------------------------------------------------------------- 
--	Scanning							--
-------------------------------------------------------------------------- 

example5 = lines >.> map read >.> scanl1' (+) 0 >.> map show >.> unlines

oneStep :: Int -> (Int,Int) -> (Int,Int)

oneStep val (n,total) = (n+1,total+val)

example6 = lines >.> map read >.> 
	   scanl1' oneStep (0,0) >.> 
	   map show >.> unlines

-------------------------------------------------------------------------- 
--	The average							--
-------------------------------------------------------------------------- 

aver (0,total) = 0
aver (n,total) = total `div` n

example7 = lines >.> map read >.> 
	   scanl1' oneStep (0,0) >.> map aver >.>
	   map show >.> unlines

-------------------------------------------------------------------------- 
--	Case study: the calculator					--
-------------------------------------------------------------------------- 

--------------------------------------------------------------------------
--	The type of stores.						--
--------------------------------------------------------------------------

type 
  Store = [ (Int,Var) ]
  in
  initial, 	-- :: Store
  lookup,	-- :: Store -> Var -> Int
  update	-- :: Store -> Var -> Int -> Store

-------------------------------------------------------------------------- 
--	Implementation equations.					--
-------------------------------------------------------------------------- 

initial :: Store 

initial             = []

lookup  :: Store -> Var -> Int

lookup []         v = 0
lookup ((n,w):st) v 
  | (v==w)    = n
  | otherwise = lookup st v

update  :: Store -> Var -> Int -> Store

update st v n       = (n,v):st

-------------------------------------------------------------------------- 
--	Evaluating an expression					--
-------------------------------------------------------------------------- 

eval :: Expr -> Store -> Int

eval (Lit n) st = n
eval (Var v) st = lookup st v
eval (Op op e1 e2) st
  = opValue op v1 v2
    where
    v1 = eval e1 st
    v2 = eval e2 st

-------------------------------------------------------------------------- 
--	opValue returns the function associated with an operator.	--
-------------------------------------------------------------------------- 

opValue :: Op -> (Int -> Int -> Int)

opValue = opValue 			-- dummy definition

-------------------------------------------------------------------------- 
--	The list of commands to be interpreted is given by		--
-------------------------------------------------------------------------- 

commList :: String -> [Command]

commList inp = map commandParse (lines inp)

-------------------------------------------------------------------------- 
--	To be defined...						--
-------------------------------------------------------------------------- 

commandParse :: String -> Command

commandParse = commandParse		-- dummy definition

-------------------------------------------------------------------------- 
--	Interpreting a list of commands.				--
-------------------------------------------------------------------------- 

calculate :: [Command] -> Store -> [String]

calculate ((Eval e):cs) st 
	= show (eval e st) : calculate cs st

calculate ((Assign v e):cs) st
	= ([v] ++ " = " ++ show val) : calculate cs st'
	  where
	  val = eval e st
	  st' = update st v val

calculate (Null:cs) st
	= "Null command" : calculate cs st

calculate [] st = []

-------------------------------------------------------------------------- 
--	The definition which gives the calculator is			--
-------------------------------------------------------------------------- 

perform :: String -> String

perform inp = unlines (calculate (commList inp) initial)


