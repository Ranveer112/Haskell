--------------------------------------------------------------------------
--                                                                      --
--	Section 2.6: Programming with Booleans				--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Exclusive or							--
-------------------------------------------------------------------------- 

exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

-------------------------------------------------------------------------- 
--	Negation							--
-------------------------------------------------------------------------- 

myNot :: Bool -> Bool

myNot True  = False
myNot False = True

-------------------------------------------------------------------------- 
--      Exclusive or again.                                             --
-------------------------------------------------------------------------- 

exOr' :: Bool -> Bool -> Bool

exOr' True  x = not x
exOr' False x = x

-------------------------------------------------------------------------- 
--	Zero sales in a week?						--
-------------------------------------------------------------------------- 

isZeroWeek :: Int -> Bool

isZeroWeek n = (sales n == 0)

sales :: Int -> Int	-- declaration of the sales function

sales = sales		-- need to redefine this to make	
			-- it a sensible definition

-------------------------------------------------------------------------- 
--	Zero sales for a week during a given period?			--
-------------------------------------------------------------------------- 

zeroInPeriod :: Int -> Bool

zeroInPeriod 0 = isZeroWeek 0
zeroInPeriod n = zeroInPeriod (n-1) || isZeroWeek n

