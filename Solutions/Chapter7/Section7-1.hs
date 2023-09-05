--------------------------------------------------------------------------
--                                                                      --
--	Section 7.1: Function composition.				--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Revisiting the filling example.					--
-------------------------------------------------------------------------- 

type Word = String
type Line = [String]

fill :: String -> [Line]

fill = splitLines . splitWords

--------------------------------------------------------------------------
--	Forward composition						--
--------------------------------------------------------------------------

infixl 9 >.>

(>.>) :: (t -> u) -> (u -> v) -> (t -> v)

f >.> g = g . f

--------------------------------------------------------------------------
--	Filling using forward composition				--
--------------------------------------------------------------------------

fill' :: String -> [Line]

fill' = splitWords >.> splitLines

--------------------------------------------------------------------------
--      Dummy definitions						--
--------------------------------------------------------------------------

splitWords :: String -> [Word]

splitWords = splitWords 		-- dummy definition

splitLines :: [Word] -> [Line]

splitLines = splitLines 		-- dummy definition

--------------------------------------------------------------------------
--	Successor function						--
--									--
--	This appears in the Haskell 1.3 prelude, and should be 		--
--	commented out here for Haskell 1.3 and Hugs0.			--
--------------------------------------------------------------------------

succ :: Int -> Int

succ n = n+1
