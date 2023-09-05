--------------------------------------------------------------------------
--                                                                      --
--	 Section 14.3, part1: Monads for Functional Programming         --
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

import Prelude hiding ((>>=),return)

--------------------------------------------------------------------------
--       Type of trees							--
--------------------------------------------------------------------------

data Tree t = Nil | Node t (Tree t) (Tree t)

--------------------------------------------------------------------------
--       A direct computation of the sum of a tree.			--
--------------------------------------------------------------------------

sTree :: Tree Int -> Int

sTree Nil = 0

sTree (Node n t1 t2) = n + sTree t1 + sTree t2

--------------------------------------------------------------------------
--       A monadic computation of the sum of a tree.			--
--------------------------------------------------------------------------

sumTree :: Tree Int -> St Int

sumTree Nil = return 0

sumTree (Node n t1 t2)
  = return n  >>= \num ->    		-- num := n
    sumTree t1  >>= \s1 ->	    	-- s1  := sumTree t1
    sumTree t2  >>= \s2 ->     		-- s2  := sumTree t2
    return (num + s1 + s2)        	-- return(num + s1 + s2)

--------------------------------------------------------------------------
--	 The monad in question -- the identity monad			--
--------------------------------------------------------------------------

type St t = t

return :: t -> St t

return = id

(>>=) :: St t -> (t -> St u) -> St u

(>>=) x f = f x

extract :: St t -> t

extract = id

