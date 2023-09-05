--------------------------------------------------------------------------
--                                                                      --
--	Section 13.6: Infinite lists					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Simple examples							--
-------------------------------------------------------------------------- 

ones :: [Int]

ones = 1 : ones

addFirstTwo :: [Int] -> Int

addFirstTwo (a:b:x) = a+b

exam1 = addFirstTwo ones

exam2,exam3 :: [Int]

exam2 = [3 .. ] 
exam3 = [3,5 .. ] 

from :: Int -> [Int]

from n       = n : from (n+1)

fromStep :: Int -> Int -> [Int]

fromStep n m = n : fromStep (n+m) m

exam4 = fromStep 3 2

-------------------------------------------------------------------------- 
--	Pythagorean triples						--
-------------------------------------------------------------------------- 

pythagTriples :: [(Int,Int,Int)]

pythagTriples =
  [ (a,b,c) | c <- [2 .. ] , b <- [2 .. c-1] , a <- [2 .. b-1] ,
              a*a + b*b == c*c ]

-------------------------------------------------------------------------- 
--	The powers of a number are given by				--
-------------------------------------------------------------------------- 

powers :: Int -> [Int]

powers n = [ n^x | x <- [0 .. ] ] 

-------------------------------------------------------------------------- 
--	Form the list x, f x, f (f x), ...				--
-------------------------------------------------------------------------- 

--	iterate :: (t -> t) -> t -> [t]

--	iterate f x = x : iterate f (f x)

-------------------------------------------------------------------------- 
--	Example: generating prime numbers				--
-------------------------------------------------------------------------- 

primes :: [Int]

primes      = sieve [2 .. ]

sieve (a:x) = a : sieve [ y | y <- x , y `mod` a > 0]

-------------------------------------------------------------------------- 
--	Testing membership of an ordered list.				--
-------------------------------------------------------------------------- 

memberOrd :: Ord t => [t] -> t -> Bool

memberOrd (m:x) n
  | m<n 	= memberOrd x n
  | m==n 	= True
  | otherwise  	= False

-------------------------------------------------------------------------- 
--	Example: Generating random numbers				--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Getting the next number in the pseudo-random sequence.		--
-------------------------------------------------------------------------- 

nextRand :: Int -> Int

nextRand n = (multiplier*n + increment) `mod` modulus

-------------------------------------------------------------------------- 
--	Iterate the nextRand function to get a pseudo-random sequence 	--
-------------------------------------------------------------------------- 

randomSequence :: Int -> [Int]

randomSequence = iterate nextRand

-------------------------------------------------------------------------- 
--	Starting values.						--
-------------------------------------------------------------------------- 

seed,multiplier,increment,modulus :: Int

seed       = 17489
multiplier = 25173
increment  = 13849
modulus    = 65536

-------------------------------------------------------------------------- 
--	Scaling the sequence so that the numbers lie between a and b.	--
-------------------------------------------------------------------------- 

scaleSequence :: Int -> Int -> [Int] -> [Int]

scaleSequence a b
  = map scale
    where
    scale n = n `div` denom + a
    range   = b-a+1
    denom   = modulus `div` range

-------------------------------------------------------------------------- 
--	Turn a distribution into a function, to assist in transforming	--
--	ramdom sequences so that they come from a distribution.		--
-------------------------------------------------------------------------- 

makeFunction :: [(t,Float)] -> (Float -> t)

makeFunction dist = makeFun dist 0.0

makeFun ((ob,p):dist) nLast rand
  | nNext >= rand && rand > nLast 
    = ob
  | otherwise 
    = makeFun dist nNext rand
      where
      nNext = p*fromInteger modulus + nLast

-------------------------------------------------------------------------- 
--	Example using the transformation.				--
-------------------------------------------------------------------------- 

exam5 = map (makeFunction dist . fromInteger) (randomSequence seed)

dist :: [(Int,Float)]

dist = [ (1,0.2) , (2,0.25) , (3,0.25) , (4,0.15) , (5,0.1) , (6,0.05) ]

-------------------------------------------------------------------------- 
-- 	Pitfall -- infinite list generators				--
--									--
--	Getting pythagTriples wrong!!					--
-------------------------------------------------------------------------- 

pythagTriples2 :: [(Int,Int,Int)]

pythagTriples2 =
  [ (a,b,c) | a <- [2 .. ] , b <- [a+1 .. ] , c <- [b+1..] ,
              a*a + b*b == c*c ]


