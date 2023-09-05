--------------------------------------------------------------------------
--                                                                      --
--	Section 15.2: The complexity of calculations			--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Factorial							--
-------------------------------------------------------------------------- 


fac :: Int -> Int

fac 0 = 1
fac n = n * fac (n-1)

-------------------------------------------------------------------------- 
--	Insertion sort							--
-------------------------------------------------------------------------- 

iSort []    = []
iSort (a:x) = ins a (iSort x)

ins a [] = [a]
ins a (b:y) | (a<=b) = a:b:y
            | otherwise = b:ins a y

-------------------------------------------------------------------------- 
--	Quicksort							--
-------------------------------------------------------------------------- 

qSort []    = []
qSort (a:x) = qSort [y|y<-x,y<=a] ++[a]++ qSort [y|y<-x,y>a]

-------------------------------------------------------------------------- 
--	Reversing functions						--
-------------------------------------------------------------------------- 

rev1 []    = []
rev1 (a:x) = rev1 x ++ [a]

rev2          = shunt []

shunt x []    = x
shunt x (a:y) = shunt (a:x) y

-------------------------------------------------------------------------- 
--	Multiplication by repeated addition.				--
-------------------------------------------------------------------------- 

mult n 0 = 0
mult n m = mult n (m-1) + n

-------------------------------------------------------------------------- 
--	`Russian' multiplication 					--
-------------------------------------------------------------------------- 

russ n 0 = 0
russ n m | (m `mod` 2 == 0) =  russ (n*n) (m `div` 2)
         | otherwise = (russ (n*n) (m `div` 2))*n

-------------------------------------------------------------------------- 
--	Merge sort							--
-------------------------------------------------------------------------- 

mSort l 
  | (len < 2) 	= l
  | otherwise 	= mer (mSort (take m l)) (mSort (drop m l))
    where 
    len = length l
    m   = len `div` 2

mer (a:x) (b:y) 
  | (a<=b) 	= a : mer x (b:y)
  | otherwise 	= b : mer (a:x) y
mer (a:x) []    = (a:x)
mer []    y     = y

