
-------------------------------------------------------------------------- 
--									--
--      Section 2.2: Programming with integers                          --
--									--
--	(c) Simon Thompson, 1995.					--
--									--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	The function sales is specified to be a function from numbers	--
--	to numbers.							--
-------------------------------------------------------------------------- 

sales :: Int -> Int

sales n = n `mod` 2 + (n+1) `mod` 3	-- The test function of exercise 2.7


-------------------------------------------------------------------------- 
--	Total sales for a period.					--
-------------------------------------------------------------------------- 

totalSales :: Int -> Int

totalSales n
  | n==0 	= sales 0
  | otherwise 	= totalSales (n-1) + sales n

-------------------------------------------------------------------------- 
--	Maximum sales in a period.					--
-------------------------------------------------------------------------- 

maxSales :: Int -> Int

maxSales n
  | n==0 			  = sales 0
  | maxSales (n-1) >= sales n     = maxSales (n-1)
  | otherwise 			  = sales n

-------------------------------------------------------------------------- 
--	Alternative definition of maximum sales function.		--
-------------------------------------------------------------------------- 

maxSales' :: Int -> Int

maxSales' n
  | n==0 	= sales 0
  | otherwise 	= maxi (maxSales' (n-1)) (sales n)

--------------------------------------------------------------------------
--	    Alternative definition of totalSales'			--
--------------------------------------------------------------------------

totalSales' :: Int -> Int

totalSales' n
  | n==0	= sales 0
  | n>0         = totalSales' (n-1) + sales n
  | otherwise   = 0

-------------------------------------------------------------------------- 
--	To use the definition of the maxi function, include the file	--
--	containing the appropriate definitions. See Chapter 11 for	--
--	further details of how files may be included in each other.	--
-------------------------------------------------------------------------- 

maxi :: Int -> Int -> Int
maxi n m 
  | n>=m 	= n
  | otherwise 	= m

