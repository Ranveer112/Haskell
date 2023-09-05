--------------------------------------------------------------------------
--                                                                      --
--	 Section 12.2: The Gofer and Hugs abstype mechanism.            --
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                        						--
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	Add the line                                                    --
--		import Prelude hiding (lookup)				--
--	for use in Hugs0 and Haskell 1.3.				--
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Var is the type of variables.					--
-------------------------------------------------------------------------- 

type Var = Char

-------------------------------------------------------------------------- 
--      Restricted synonym for the Store type.				--
-------------------------------------------------------------------------- 

type 
  Store = (Var -> Int)
  in
  initial, 	-- :: Store
  lookup,	-- :: Store -> Var -> Int
  update	-- :: Store -> Var -> Int -> Store

-------------------------------------------------------------------------- 
--      Implementing the store as a function from variables to numbers	--
-------------------------------------------------------------------------- 

initial :: Store

initial v = 0

lookup  :: Store -> Var -> Int

lookup st v = st v

update  :: Store -> Var -> Int -> Store

update st v n w 
  | v==w    	= n
  | otherwise 	= st w

