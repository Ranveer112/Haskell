module Main (main) where

import Types ( Tree(Leaf,Node), Bit(L,R), HCode(..) , Table(..)	 )
import Coding 
import MakeCode ( codes, codeTable )

main = print decoded

--------------------------------------------------------------------------
--	Examples							--
--------------------------------------------------------------------------

table2 = codeTable (codes "there is a green hill")
tree2 = codes "there is a green hill"
message = "there are green hills here"
coded = codeMessage table2 message
decoded = decodeMessage tree2 coded
