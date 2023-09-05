--------------------------------------------------------------------------
--									--
--	Frequency.hs							--
--									--
--	Calculating the frequencies of words in a text, used in 	--
--	Huffman coding.							--
--									--
--	(c) Simon Thompson, 1995.					--
--									--
--------------------------------------------------------------------------

module Frequency ( frequency ) where

--------------------------------------------------------------------------
--	Calculate the frequencies of characters in a list.		--
--									--
--	This is done by sorting, then counting the number of		--
--	repetitions. The counting is made part of the merge 		--
--	operation in a merge sort.					--
--------------------------------------------------------------------------

frequency :: [Char] -> [ (Char,Int) ]

frequency
  = mergeSort freqMerge . mergeSort alphaMerge . map start
    where
    start ch = (ch,1)

--------------------------------------------------------------------------
--	Merge sort parametrised on the merge operation. This is more	--
--	general than parametrising on the ordering operation, since	--
--	it permits amalgamation of elements with equal keys		--
--	for instance.							--
--------------------------------------------------------------------------
 
mergeSort :: ([a]->[a]->[a]) -> [a] -> [a]

mergeSort merge x
  | length x < 2 	= x					
  | otherwise		 = merge (mergeSort merge first)
            		         (mergeSort merge second)	
                  where
                  first = take half x
                  second = drop half x
                  half = (length x) `div` 2

--------------------------------------------------------------------------
--	Order on first entry of pairs, with				--
--	accumulation of the numeric entries when equal first entry.	--
--------------------------------------------------------------------------

alphaMerge x [] = x
alphaMerge [] y = y
alphaMerge ((a,n):x) ((b,m):y)
  | (a==b) 	= (a,n+m) : alphaMerge x y		
  | (a<b) 	= (a,n) : alphaMerge x ((b,m):y)	
  | otherwise 	= (b,m) : alphaMerge ((a,n):x) y	

--------------------------------------------------------------------------
--	Lexicographic ordering, second field more significant.		--
--------------------------------------------------------------------------

freqMerge x [] = x
freqMerge [] y = y
freqMerge ((a,n):x) ((b,m):y)
  | (n<m || (n==m && a<b)) 
    = (a,n) : freqMerge x ((b,m):y)	
  | otherwise 
    = (b,m) : freqMerge ((a,n):x) y	
