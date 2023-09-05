--------------------------------------------------------------------------
--                                                                      --
--	Section 13.7: Why infinite lists?				--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

module WhyInfLists where

-------------------------------------------------------------------------- 
--	Running sums of a list of numbers -- the process view.		--
-------------------------------------------------------------------------- 

listSums :: [Int] -> [Int]

listSums iList = out
                 where
                 out = 0 : zipWith (+) iList out

examSum = listSums [1 .. ]

-------------------------------------------------------------------------- 
--	Scanning along a list.						--
-------------------------------------------------------------------------- 

scanl1' :: (t -> u -> u) -> u -> [t] -> [u]

scanl1' f st iList
  = out
    where
    out = st : zipWith f iList out


