--------------------------------------------------------------------------
--                                                                      --
--	Section 2.11: Function definitions				--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Highest common factor						--
-------------------------------------------------------------------------- 

hcf :: Int -> Int -> Int
hcf n m
  | m == n	= n
  | m > n	= hcf m n
  | otherwise	= hcf (n-m) m

-------------------------------------------------------------------------- 
--	Summing the squares of two numbers.				--
-------------------------------------------------------------------------- 

sumSquares :: Int -> Int -> Int

sumSquares n m 
  = sqN + sqM
    where
    sqN = n*n
    sqM = m*m

-------------------------------------------------------------------------- 
--	Printing an average.						--
-------------------------------------------------------------------------- 

printAverage :: Int -> String

printAverage n 
  = text ++ averageVal
    where
    text       = "\n     Average " 
    averageVal = rPrintFloat (averageSales n)

rPrintFloat  :: Float -> String

rPrintFloat = rPrintFloat		-- dummy definition

averageSales :: Int -> Float 

averageSales = averageSales		-- dummy definition

-------------------------------------------------------------------------- 
--	Are numbers even? odd?						--
-------------------------------------------------------------------------- 

isOdd,isEven :: Int -> Bool

isOdd 0  = False
isOdd n  = isEven (n-1)

isEven 0 = True
isEven n = isOdd (n-1)

-------------------------------------------------------------------------- 
--	Which number has the larger square?				--
-------------------------------------------------------------------------- 

maxsq x y 
  | sqx > sqy 	= sqx
  | otherwise 	= sqy
      where
      sqx  = sq x
      sqy  = sq y

      sq :: Int -> Int
      sq z = z*z

