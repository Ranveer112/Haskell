--------------------------------------------------------------------------
--									--
--	Types.hs							--
--									--
--	The types used in the Huffman coding example.			--
--									--
--	(c) Simon Thompson, 1995.					--
--									--
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	The interface to the module Types is written out		--
-- 	explicitly here, after the module name.                    	--
--------------------------------------------------------------------------

module Types ( Tree(Leaf,Node), Bit(L,R), HCode(..) , Table(..)  ) 
  where

--------------------------------------------------------------------------
--	Trees to represent the relative frequencies and therefore	--
--	the Huffman codes.						--
--------------------------------------------------------------------------

data Tree = Leaf Char Int | Node Int Tree Tree

--------------------------------------------------------------------------
--	The types of bits, Huffman codes and tables of Huffman codes.	--
--------------------------------------------------------------------------

data Bit = L | R

type HCode = [Bit]

type Table = [ (Char,HCode) ]

