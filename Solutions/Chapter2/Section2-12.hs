--------------------------------------------------------------------------
--                                                                      --
--	Section 2.12: Programming with local definitions		--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	How many times does the maximum occur?				--
-------------------------------------------------------------------------- 

maxThreeOccurs :: Int -> Int -> Int -> (Int,Int)

maxThreeOccurs n m p
  = (max,eqCount)
    where
    max     = maxiThree n m p
    eqCount = equalCount max n m p

-------------------------------------------------------------------------- 
--	Maximum of three						--
-------------------------------------------------------------------------- 

maxiThree :: Int -> Int -> Int -> Int

maxiThree a b c = max a (max b c)

-------------------------------------------------------------------------- 
--	 How many times does a number occur?				--
-------------------------------------------------------------------------- 

equalCount :: Int -> Int -> Int -> Int -> Int

equalCount val n m p
  = isN + isM + isP
    where
    isN = if n==val then 1 else 0
    isM = if m==val then 1 else 0
    isP = if p==val then 1 else 0

-------------------------------------------------------------------------- 
--	Alternatively ....						--
-------------------------------------------------------------------------- 

equalCount' :: Int -> Int -> Int -> Int -> Int

equalCount' val n m p
  = isVal n  + isVal m + isVal p
    where
    isVal :: Int -> Int
    isVal x  = if x==val then 1 else 0



