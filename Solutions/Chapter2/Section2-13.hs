--------------------------------------------------------------------------
--                                                                      --
--	Section 2.13: Quadratic equations				--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Finding the single root,					--
-------------------------------------------------------------------------- 

oneRoot :: Float -> Float -> Float -> Float

oneRoot a b c = -b/(2.0*a)

-------------------------------------------------------------------------- 
--	... and two real roots.						--
-------------------------------------------------------------------------- 

twoRoots :: Float -> Float -> Float -> (Float,Float)

twoRoots a b c
  = (d-e,d+e)
    where
    d = -b/(2.0*a)
    e = sqrt(b^2-4.0*a*c)/(2.0*a)

-------------------------------------------------------------------------- 
--	Top-level function.						--
-------------------------------------------------------------------------- 

quadAnalyse :: Float -> Float -> Float -> String

quadAnalyse a b c
  = header a b c ++ roots a b c

-------------------------------------------------------------------------- 
--	Printing the header.						--
-------------------------------------------------------------------------- 

header :: Float -> Float -> Float -> String

header a b c
  = "The quadratic equation\n\n\t" ++
    show a ++ "*X^2 + " ++ 
    show b ++ "*X + " ++
    show c ++ " = 0" ++ "\n\nhas "

-------------------------------------------------------------------------- 
--	Printing the roots.						--
-------------------------------------------------------------------------- 

roots :: Float -> Float -> Float -> String

roots a b c
  | b^2 > 4.0*a*c
    = "two roots: " ++ show f ++ " " ++ show s      
  | b^2 == 4.0*a*c
    = "one root: " ++ show (oneRoot a b c)             
  | otherwise 
    = "no roots"
      where
      (f,s) = twoRoots a b c

-------------------------------------------------------------------------- 
--	Error handling version of oneRoot				--
-------------------------------------------------------------------------- 

oneRoot' :: Float -> Float -> Float -> Float

oneRoot' a b c 
  | (a /= 0.0) 	= -b/(2.0*a)
  | otherwise 	= error "oneRoot called with a==0"




