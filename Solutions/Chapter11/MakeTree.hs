--------------------------------------------------------------------------
--									--
--	makeTree.m							--
--									--
--	Turn a frequency table into a Huffman tree			--
--									--
--	(c) Simon Thompson, 1995.					--
--									--
--------------------------------------------------------------------------

module MakeTree ( makeTree ) where

import Types ( Tree(Leaf,Node), Bit(L,R), HCode(..), Table(..) )

--------------------------------------------------------------------------
--	Convert the trees to a list, then amalgamate into a single	--
--	tree.								--
--------------------------------------------------------------------------

makeTree :: [ (Char,Int) ] -> Tree

makeTree = makeCodes . toTreeList

--------------------------------------------------------------------------
--	Huffman codes are created bottom up: look for the least		--
--	two frequent letters, make these a new "isAlpha" (i.e. tree)	--
--	and repeat until one tree formed.				--
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	To tree list -- makes the initial data structure.		--
--------------------------------------------------------------------------

toTreeList :: [ (Char,Int) ] -> [ Tree ]

toTreeList = map toLeaf
	     where
	     toLeaf (c,n) = Leaf c n

--------------------------------------------------------------------------
--	The value of a tree.						--
--------------------------------------------------------------------------

value :: Tree -> Int

value (Leaf c n) = n
value (Node n t1 t2) = n

--------------------------------------------------------------------------
--	Pair two trees.							--
--------------------------------------------------------------------------

pair :: Tree -> Tree -> Tree

pair t1 t2 = Node (v1+v2) t1 t2
	     where
	     v1 = value t1
	     v2 = value t2

--------------------------------------------------------------------------
--	Insert a tree in a list of trees sorted by ascending value.	--
--------------------------------------------------------------------------

insTree :: Tree -> [Tree] -> [Tree]

insTree t [] = [t]
insTree t (t1:ts) 
  | (value t <= value t1) 
    = t:t1:ts		
  | otherwise 
    = t1 : insTree t ts	
	
--------------------------------------------------------------------------
--	Amalgamate the front two elements of the list of trees.		--
--------------------------------------------------------------------------

amalgamate :: [ Tree ] -> [ Tree ]

amalgamate ( t1 : t2 : ts )
	= insTree (pair t1 t2) ts

--------------------------------------------------------------------------
--	Make codes: amalgamate the whole list.				--
--------------------------------------------------------------------------

makeCodes :: [Tree] -> Tree

makeCodes [t] = t
makeCodes ts = makeCodes (amalgamate ts) 

