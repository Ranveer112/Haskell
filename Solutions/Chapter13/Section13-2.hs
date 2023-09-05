--------------------------------------------------------------------------
--                                                                      --
--	Section 13.2: Calculation Rules					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------


-------------------------------------------------------------------------- 
--	An example with pattern matching.				--
-------------------------------------------------------------------------- 

f1 :: [Int] -> [Int] -> Int
f1 [] y        = 0 
f1 (a:x) []    = 0
f1 (a:x) (b:y) = a+b

exam1 = f1 [1 .. 3] [1 .. 3]

-------------------------------------------------------------------------- 
--	An example with guards.						--
-------------------------------------------------------------------------- 

f2 :: Int -> Int -> Int -> Int 
f2 a b c 
  | a>=b && a>=c 	= a
  | b>=a && b>=c 	= b
  | otherwise 		= c

exam2 = f2 (2+3) (4-1) (3+9)

-------------------------------------------------------------------------- 
--	An example with local definitions.				--
-------------------------------------------------------------------------- 

f3 :: Int -> Int -> Int

f3 a b 
  | notNil l 	= front l
  | otherwise 	= b
         where
         l = [a .. b]

front :: [Int] -> Int

front (c:d:y) = c+d
front [c]     = c

notNil []    = False
notNil (a:x) = True

exam3 = f3 3 5
