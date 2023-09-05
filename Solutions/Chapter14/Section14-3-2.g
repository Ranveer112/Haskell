--------------------------------------------------------------------------
--                                                                      --
--	Section 14.3, part2: Monads for Functional Programming          --
--	Gofer version							--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--       Type of trees							--
--------------------------------------------------------------------------

data Tree t = Nil | Node t (Tree t) (Tree t)

--------------------------------------------------------------------------
--       A state monad							--
--------------------------------------------------------------------------

type State t u = Table t -> (Table t , u)

type Table t = [t]

return :: u -> State t u

return x = \tab -> (tab,x)

(>>=) :: State t u -> (u -> State t v) -> State t v

st >>= f 
  = \tab -> let 
	    (newTab,y) = st tab 
	    in
	    f y newTab

extract :: State t u -> u

extract st = snd (st [])

--------------------------------------------------------------------------
--	 Assigning unique natural numbers to the members of a tree.	--
--------------------------------------------------------------------------

numTree :: Eq t => Tree t -> Tree Int
numTree = extract . numberTree

numberTree :: Eq t => Tree t -> State t (Tree Int)

numberTree Nil = return Nil

numberTree (Node x t1 t2)
  = numberNode x  >>= \num ->
    numberTree t1 >>= \nt1 ->
    numberTree t2 >>= \nt2 ->
    return (Node num nt1 nt2)

--------------------------------------------------------------------------
--      Numbering a Node involves a lookup, which in turn will modify 	--
--	the state in case the value is seen for the first time.		--
--------------------------------------------------------------------------

numberNode :: Eq t => t -> State t Int

numberNode x table
  | elem x table	= (table , lookup x table)
  | otherwise		= (table++[x] , length table)

lookup :: Eq t => t -> Table t -> Int

lookup x table = look x table 0

look :: Eq t => t -> Table t -> Int -> Int

look x [] n = error "table lookup"
look x (y:ys) n
  | x==y	= n
  | otherwise	= look x ys (n+1)

--------------------------------------------------------------------------
--	Examples							--
--------------------------------------------------------------------------

example :: Tree Char

example = Node 'z' ex1 ex2

ex1 = Node 'f' ex2 ex2

ex2 = Node 'q' (Node 'z' Nil Nil) (Node 'e' Nil Nil)

data Children = Ahmet | Dweezil | Moon 

instance Eq Children where
  Ahmet   == Ahmet   = True
  Dweezil == Dweezil = True
  Moon    == Moon    = True
  _       == _       = False

zapTree :: Tree Children

zapTree = Node Moon (Node Ahmet Nil Nil)
		    (Node Dweezil (Node Ahmet Nil Nil)
				  (Node Moon Nil Nil))

