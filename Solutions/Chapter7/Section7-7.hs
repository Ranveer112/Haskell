--------------------------------------------------------------------------
--                                                                      --
--	Section 7.7: Design revisited					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Sorting with the comparison function as a parameter.		--
-------------------------------------------------------------------------- 

sortG :: (t -> t -> Bool) -> [t] -> [t]

sortG comp (a:x)
  = sortG comp smaller ++ [a] ++ sortG comp larger
    where
    smaller = [ b | b<-x , comp b a ]
    larger  = [ b | b<-x , comp a b ]

