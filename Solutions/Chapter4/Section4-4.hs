--------------------------------------------------------------------------
--                                                                      --
--	Section 4.4. Pattern Matching					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	Getting the first digit in a string.					--
--------------------------------------------------------------------------

firstDigit :: String -> Char

firstDigit st = case (digits st) of
			[]    -> '0'
			(a:x) -> a

-------------------------------------------------------------------------- 
--	Getting the digits in a string. (From Section 4.3)		--
--	As in 4.3, isDigit needs to be added for Hugs0			--
-------------------------------------------------------------------------- 

digits :: String -> String

digits [] = []
digits (first:rest) 
  | isDigit first 	= first : digits rest
  | otherwise 		= digits rest

-------------------------------------------------------------------------- 
--	Summing a list of pairs.					--
--									--
--	sumPairs :: [(Int,Int)] -> Int					--
--									--
-------------------------------------------------------------------------- 

sumPairs1 [] = 0
sumPairs1 (a:x) = fst a + snd a + sumPairs1 x 

sumPairs2 [] = 0
sumPairs2 (a:x) = c + d + sumPairs2 x 
                  where
                  c = fst a
                  d = snd a

sumPairs3 [] = 0
sumPairs3 (a:x) = c + d + sumPairs3 x 
                  where
                  (c,d) = a

sumPairs4 [] = 0
sumPairs4 ((c,d):x) = c + d + sumPairs4 x 

sumPairs5 [] = 0
sumPairs5 (a:x) = sumPair a + sumPairs5 x

-------------------------------------------------------------------------- 
--	Summing a pair.							--
-------------------------------------------------------------------------- 

sumPair :: (Int,Int) -> Int

sumPair  (c,d) = c+d

-------------------------------------------------------------------------- 
--	zip is built in							--
--									--
--	zip (a:x) (b:y) = (a,b) : zip x y				--
--	zip _ _         = []						--
-------------------------------------------------------------------------- 


-------------------------------------------------------------------------- 
--	Alternative definition of zip.					--
-------------------------------------------------------------------------- 

zipNew :: [Int] -> [Int] -> [(Int,Int)]

zipNew (a:x) (b:y) = (a,b) : zipNew x y
zipNew (a:x) []    = []
zipNew []    y     = []

