--------------------------------------------------------------------------
--                                                                      --
--	Chapter 9: Checking types					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	The examples from the start of the Section on			--
--	Polymorphic types (Section 9.2).				--
--------------------------------------------------------------------------

-- f :: (t,Int) -> (t,[Int])

f (a,c) = (a,['a' .. c])

-- g :: (Int,[u]) -> Int

g (m,l) = m + length l

h = g.f

-------------------------------------------------------------------------- 
--	The example from the end of Section 9.3.			--
-------------------------------------------------------------------------- 

ex1 = length (empty ++ id [True,False]) + length (empty ++ id [2,3,4])
      where 
      empty = []
      id w  = w

-------------------------------------------------------------------------- 
--	From the exercises of Sections 9.3 and 9.4.			--
-------------------------------------------------------------------------- 

f' x b l 
  | (tester l b) 
    = appendL x ++ appendL []
  | otherwise 
    = appendL l
    where
    appendL x = x++l
    tester x b = b && ( length l > length x)

mergE (a:x) (b:y) 
  | (a<b) 
    = a : mergE x (b:y)
  | (a==b) 
    = a : mergE x y
  | otherwise 
    = b : mergE (a:x) y
mergE (a:x) []    = (a:x)
mergE []    (b:y) = (b:y)
mergE []    []    = []

exam1 = ([[]],[[]])

exam2 = (empList,empList)
        where
        empList = [[]]

exam3 = double [[]]
        where
        double x = (x,x)

-------------------------------------------------------------------------- 
--	Examples using special functions.				--
-------------------------------------------------------------------------- 

-- exam4 = show (id.id , 2 `div` 1) 

-------------------------------------------------------------------------- 
--	Using show in an acceptable way.				--
-------------------------------------------------------------------------- 

printNum :: Int -> String

printNum obj = show obj ++ "\n"

