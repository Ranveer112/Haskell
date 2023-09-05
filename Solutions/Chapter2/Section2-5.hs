--------------------------------------------------------------------------
--                                                                      --
--	Section 2.5: Definitions: Patterns				--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

sales :: Int -> Int

sales = sales	-- for the file to be usable, please
		-- include a sensible defintion for sales.

-------------------------------------------------------------------------- 
--	Redefining totalSales ... 					--
-------------------------------------------------------------------------- 

totalSales :: Int -> Int

totalSales 0 = sales 0 
totalSales n = totalSales (n-1) + sales n

-------------------------------------------------------------------------- 
--	A wild card in a check for zero					--
-------------------------------------------------------------------------- 

isZero :: Int -> Bool

isZero 0 = True
isZero _ = False

-------------------------------------------------------------------------- 
--	Fibonacci numbers						--
-------------------------------------------------------------------------- 

fib :: Int -> Int

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

