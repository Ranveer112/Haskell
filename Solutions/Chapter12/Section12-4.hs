--------------------------------------------------------------------------
--                                                                      --
--	Section 12.4: The Haskell ADT mechanism.			--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------


module Store ( StoreT , initial , lookup , update , eqStore ) 
where

import Prelude hiding (lookup)
-------------------------------------------------------------------------- 
--	Var is the type of variables.					--
-------------------------------------------------------------------------- 

type Var = Char

-------------------------------------------------------------------------- 
--	The synonym is only visible in the definitions of		--
--		initial, lookup and update.				--
-------------------------------------------------------------------------- 

data 
  StoreT = StoreC [ (Int,Var) ] 

--  initial 	:: StoreT,
--  lookup	:: StoreT -> Var -> Int,
--  update	:: StoreT -> Var -> Int -> StoreT,
--  eqStore 	:: StoreT -> StoreT -> Bool


instance Eq StoreT where 
  (==) = eqStore

eqStore :: StoreT -> StoreT -> Bool

eqStore = (==)

-------------------------------------------------------------------------- 
--	Implementation equations.					--
-------------------------------------------------------------------------- 


initial :: StoreT 

initial = StoreC []

lookup  :: StoreT -> Var -> Int

lookup (StoreC [])         v = 0
lookup (StoreC ((n,w):st)) v 
  | v==w      = n
  | otherwise = lookup (StoreC st) v

update  :: StoreT -> Var -> Int -> StoreT

update (StoreC st) v n = StoreC ((n,v):st)

