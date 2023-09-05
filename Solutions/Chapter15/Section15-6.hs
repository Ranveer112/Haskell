--------------------------------------------------------------------------
--                                                                      --
--	Section 15.6 Avoiding re-computation: memoization		--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Fibonacci numbers						--
-------------------------------------------------------------------------- 

fib :: Int -> Int

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-------------------------------------------------------------------------- 
--	A Fibonacci function returning a pair, this and the next number	--
--	as it were.							--
-------------------------------------------------------------------------- 

fibP :: Int -> (Int,Int)

fibP 0 = (0,1)
fibP n = (b,a+b)
             where
             (a,b) = fibP (n-1)

-------------------------------------------------------------------------- 
--	The list of Fibonacci numbers					--
-------------------------------------------------------------------------- 

fibs :: [Int]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-------------------------------------------------------------------------- 
--	Dynamic programming						--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Three algoirthms to find the length of a maximal 		--
--	common subsequence						--
-------------------------------------------------------------------------- 

mLen :: Eq t => [t] -> [t] -> Int

mLen x []        = 0
mLen [] y        = 0
mLen (a:x) (b:y) 
  | a==b 	= 1 + mLen x y
  | otherwise	= max (mLen x (b:y)) (mLen (a:x) y)

maxLen :: Eq t => [t] -> [t] -> Int -> Int -> Int

maxLen l m 0 j = 0 
maxLen l m i 0 = 0
maxLen l m i j
  | l!!(i-1) == m!!(j-1)  	= (maxLen l m (i-1) (j-1)) + 1		
  | otherwise			= max (maxLen l m i (j-1)) (maxLen l m (i-1) j)

maxTab ::  Eq t => [t] -> [t] -> [[Int]]

maxTab l m
  = result
    where 
    result = [0,0 .. ] : zipWith f [0 .. ] result
    f i prev  
        = ans
          where
          ans   = 0 : zipWith g [0 .. ] ans
          g j v 
	    | l!!i == m!!j	= prev!!j +1
            | otherwise		= max v (prev!!(j+1))

