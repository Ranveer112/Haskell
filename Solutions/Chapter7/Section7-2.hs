--------------------------------------------------------------------------
--                                                                      --
--	Section 7.2: Functions as results				--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

infixl 9 >.>

f >.> g = g . f

-------------------------------------------------------------------------- 
--	Apply a function twice.						--
-------------------------------------------------------------------------- 

twice :: (t -> t) -> (t -> t)

twice f = f.f

exam1 :: Int

exam1 = (twice succ) 12

--------------------------------------------------------------------------
--	succ should be commented out in Haskell 1.3 and Hugs0.		--
--------------------------------------------------------------------------

succ :: Int -> Int

succ n = n+1

-------------------------------------------------------------------------- 
--	Apply the function n times.					--
-------------------------------------------------------------------------- 

iter :: Int -> (t -> t) -> (t -> t)

iter 0 f = id
iter n f = f >.> iter (n-1) f

-------------------------------------------------------------------------- 
--	Return a function adding the given number to its argument.	--
-------------------------------------------------------------------------- 

addNum :: Int -> (Int -> Int)
addNum n = h
           where
           h m = n+m

addNum' :: Int -> (Int -> Int)
addNum' n = (\m -> n+m)

-------------------------------------------------------------------------- 
--	Apply f to the two values before sending to g.			--
-------------------------------------------------------------------------- 

comp2 f g = (\x y -> g (f x) (f y))

exam2 = comp2 sq add 3 4

sq :: Int -> Int

sq n = n*n

add :: Int -> Int -> Int

add a b = a+b

-------------------------------------------------------------------------- 
--	Exercise functions.						--
-------------------------------------------------------------------------- 

-- total :: (Int -> Int) -> (Int -> Int)
-- slope :: (Int -> Int) -> (Int -> Int)
-- integrate :: (Int -> Int) -> (Int -> Int -> Int)
 
