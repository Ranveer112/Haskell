--------------------------------------------------------------------------
--                                                                      --
--	Section 4.7: Text processing.					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
-- 									--
--	getLine is already defined in the Haskell 1.3 prelude. To 	--
--	a definition clash add the line					--
--		import Prelude hiding (getLine)				--
--	to avoid problems in Haskell 1.3 and Hugs0.			--
--------------------------------------------------------------------------

-- type String = [Char]

-------------------------------------------------------------------------- 
--	Example text.							--
-------------------------------------------------------------------------- 

examT :: String

examT = "The heat bloomed     in December as the\ncarnival  season \nkicked into  gear.\nNearly helpless with sun and glare, I avoided Rio's brilliant\nsidewalks\nand glittering beaches,\npanting in dark   corners\n and waiting out the inverted southern summer."

-------------------------------------------------------------------------- 
--	The list of whitespace characters.				--
-------------------------------------------------------------------------- 


whitespace :: [Char]

whitespace = ['\n','\t',' ']

-------------------------------------------------------------------------- 
--	Getting a word.							--
-------------------------------------------------------------------------- 

getWord :: String -> String

getWord []    = [] 
getWord (a:x) 
  | elem a whitespace 	= []
  | otherwise          	= a : getWord x

exam1 = getWord "cat dog"


-------------------------------------------------------------------------- 
--	Dropping a word.						--
-------------------------------------------------------------------------- 

dropWord :: String -> String

dropWord []    = []
dropWord (a:x) 
  | elem a whitespace 	= (a:x)
  | otherwise 		= dropWord x

exam2 = dropWord "cat dog" 


-------------------------------------------------------------------------- 
--	Dropping whitespace from the front of a string.			--
--------------------------------------------------------------------------

dropSpace :: String -> String

dropSpace []    = []
dropSpace (a:x) 
  | elem a whitespace 	= dropSpace x
  | otherwise 		= (a:x)


-------------------------------------------------------------------------- 
--	Splitting into words. The top-level function calls split after	--
--	removing any white space at the sstart of the input.		--
-------------------------------------------------------------------------- 

type Word = String

splitWords :: String -> [Word]

splitWords x = split (dropSpace x)

split :: String -> [Word]

split [] = []
split x
  = (getWord x) : split (dropSpace (dropWord x))

exam3 = splitWords "  dog cat"


-------------------------------------------------------------------------- 
--	A line is a list of words.					--
-------------------------------------------------------------------------- 

type Line = [Word]

-------------------------------------------------------------------------- 
--	Getting a line of a particular length from a list of words.	--
-------------------------------------------------------------------------- 

getLine :: Int -> [Word] -> Line

getLine len []     = []

getLine len (w:ws)
  | length w <= len 	= w : restOfLine
  | otherwise 		= []
      where
      newlen      = len - (length w + 1)
      restOfLine  = getLine newlen ws

exam4 = getLine 20 ["Mary","Poppins","looks","like","Maria"]

-------------------------------------------------------------------------- 
--	Dropping a line -- companion to getLine.			--
-------------------------------------------------------------------------- 

dropLine :: Int -> [Word] -> [Word]

dropLine = dropLine		-- dummy definition

-------------------------------------------------------------------------- 
--	Splitting a list of words into a list of lines of maximum 	--
--	length lineLen.							--
-------------------------------------------------------------------------- 

lineLen :: Int

lineLen = 60				-- may be modified

splitLines :: [Word] -> [Line]

splitLines [] = []
splitLines x
  = getLine lineLen x
         : splitLines (dropLine lineLen x)

-------------------------------------------------------------------------- 
--	To fill a text, split it into words, and then into lines.	--
-------------------------------------------------------------------------- 

fill :: String -> [Line]

fill st = splitLines (splitWords st)

-------------------------------------------------------------------------- 
--	Printing lines -- left as an exercise.				--
-------------------------------------------------------------------------- 

-- printLines :: [Line] -> String
-- printLine :: Line -> String

-- exam5 = printLine [ "dog" , "cat" ] 

