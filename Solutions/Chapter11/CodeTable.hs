--------------------------------------------------------------------------
--									--
--	codeTable.m							--
--									--
--	Converting a Huffman tree to a ord table.			--
--									--
--	(c) Simon Thompson, 1995.					--
--									--
--------------------------------------------------------------------------

module CodeTable ( codeTable ) where

import Types ( Tree(Leaf,Node), Bit(L,R), HCode(..), Table(..) )


--------------------------------------------------------------------------
--	Auxiliary function used in conversion to a table.		--
--------------------------------------------------------------------------

convert :: HCode -> Tree -> Table

convert cd (Leaf c n) =  [(c,cd)]
convert cd (Node n t1 t2)
	= (convert (cd++[L]) t1) ++ (convert (cd++[R]) t2)

--------------------------------------------------------------------------
--	Making a table from a Huffman tree				--
--------------------------------------------------------------------------

codeTable :: Tree -> Table

codeTable = convert []

--------------------------------------------------------------------------
--	Printing functions						--
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	Show a tree, using indentation to show structure.		--
--									--
--	showTreeIndent has a second, current level of indentation,	--
--	parameter.							--
--------------------------------------------------------------------------

showTree :: Tree -> String

showTree t = showTreeIndent 0 t

showTreeIndent :: Int -> Tree -> String

showTreeIndent m (Leaf c n) 
	= spaces m ++ show c ++ " " ++ show n ++ "\n"
showTreeIndent m (Node n t1 t2)
	= showTreeIndent (m+4) t1 ++
	  spaces m ++ "[" ++ show n ++ "]" ++ "\n" ++
	  showTreeIndent (m+4) t2

spaces :: Int -> String

spaces n = map (\x -> ' ') [0..n-1]
--------------------------------------------------------------------------
--	Printable version of ord. 					--
--------------------------------------------------------------------------

showCode :: HCode -> String
showCode = map conv
	   where
	   conv R = 'R'
	   conv L = 'L'

--------------------------------------------------------------------------
--	Print a ord table.						--
--------------------------------------------------------------------------

showTable = concat . map showPair
		where
		showPair (c,n) = [c] ++ " " ++ showCode n ++ "\n"
