--------------------------------------------------------------------------
--                                                                      --
--	Section 12.2: The Gofer and Hugs ADT mechanism.			--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
-- 	Add the line							--
--		import Prelude hiding (lookup)				--
--	for use in Hugs0 and Haskell 1.3.				--
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Var is the type of variables.					--
-------------------------------------------------------------------------- 

type Var = Char

-------------------------------------------------------------------------- 
--	The synonym is only visible in the definitions of		--
--		initial, lookup and update.				--
-------------------------------------------------------------------------- 

type 
  Store = [ (Int,Var) ] 
  in
  initial 	:: Store,
  lookup	:: Store -> Var -> Int,
  update	:: Store -> Var -> Int -> Store,
  eqStore 	:: Store -> Store -> Bool


instance Eq Store where 
  (==) = eqStore

eqStore = (==)

-------------------------------------------------------------------------- 
--	Implementation equations.					--
-------------------------------------------------------------------------- 


-- initial :: Store 

initial = []

-- lookup  :: Store -> Var -> Int

lookup [] v         = 0
lookup ((n,w):st) v 
  | v==w      = n
  | otherwise = lookup st v

-- update  :: Store -> Var -> Int -> Store

update st v n = (n,v):st

