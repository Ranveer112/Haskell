--------------------------------------------------------------------------
--                                                                      --
--	Section 4.1: Lists in Haskell  					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------


list1 :: [Int]
list1 = [1,2,3,4,1,4] 

list2 :: [Bool]
list2 = [True]        

list3 :: [Char]
list3 = ['a','a','b'] 

list4 :: [Char]
list4 = "aab"         

list5 :: [Int -> Int]
list5 = [totalSales,totalSales]  

list6 :: [[Int]]
list6 = [ [12,2] , [2,12] , [] ] 

list7 :: [t]	-- see Section 6.2 for further details of this type
list7 = [] 

list8 :: [Int]
list8 = [2 .. 7]   

list9 :: [Float]
list9 = [3.1 .. 7.0] 

list10 :: [Int]
list10 = [7,6 .. 3]   

list11 :: [Float]
list11 = [0.0,0.3 .. 1.0] 

totalSales :: Int ->  Int

totalSales = totalSales 	-- dummy definition
				-- replace before use
