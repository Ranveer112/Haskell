--------------------------------------------------------------------------
--                                                                      --
--	Section 10.4: Case study: Program Errors			--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	The built in error function.					--
--		error :: String -> t					--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Dummy values in a division function.				--
-------------------------------------------------------------------------- 

divide :: Int -> Int -> Int

divide n m 
  | (m /= 0) 	= n `div` m
  | otherwise 	= 0

-------------------------------------------------------------------------- 
--	A dummy value from the head function is passed in as an 	--
--	extra parameter.						--
-------------------------------------------------------------------------- 

hd :: t -> [t] -> t

hd b (a:x) = a
hd b []    = b

-------------------------------------------------------------------------- 
--	Error types							--
-------------------------------------------------------------------------- 

data Err t = OK t | Error

-------------------------------------------------------------------------- 
--	Division returning a value of type err num.			--
-------------------------------------------------------------------------- 

errDiv :: Int -> Int -> Err Int

errDiv n m 
  | (m /= 0) 	= OK (n `div` m)
  | otherwise 	= Error

-------------------------------------------------------------------------- 
--	Lift a function so that it transmits errors from input to 	--
--	output.								--
-------------------------------------------------------------------------- 

lift :: (t->u)->Err t->Err u

lift g Error  = Error
lift g (OK x) = OK (g x)

-------------------------------------------------------------------------- 
--	Trapping and handling an error.					--
-------------------------------------------------------------------------- 

trap::(t->u)->u->Err t->u

trap f v (OK x) = f x
trap f v Error  = v

exam1 = trap (1+) 56 (lift (*3) (errDiv 9 0)) 
exam2 = trap (1+) 56 (lift (*3) (errDiv 9 1))  

-------------------------------------------------------------------------- 
--	 Functions from the exercises...				--
-------------------------------------------------------------------------- 

-- process :: [Int] -> Int -> Int -> Int

-- squash :: Err (Err t) -> Err t

-- composeErr :: (t -> Err u) -> (u -> Err v) -> (t -> Err v)

-------------------------------------------------------------------------- 
--	Generalising the type of errors to include an error message.	--
-------------------------------------------------------------------------- 

data NewErr t = NewOK t | NewError String



