--------------------------------------------------------------------------
--                                                                      --
--	Section 7.6: Creating an index.					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-- import Prelude hiding (compare,getLine)

--------------------------------------------------------------------------
--	When using Haskell 1.3 or Hugs0, add the line			--
--									--
--		import Prelude hiding (compare,getLine)			--
--									--
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	Forward composition (see Section7.1)				--
--------------------------------------------------------------------------

infixl 9 >.>

(>.>) :: (t -> u) -> (u -> v) -> (t -> v)

f >.> g = g . f

-------------------------------------------------------------------------- 
--	 The goal of this section; defining an index automatically.	--
-------------------------------------------------------------------------- 

makeIndex :: Doc -> [ ([Int],Word) ]

-------------------------------------------------------------------------- 
--	Type synonyms used.						--
-------------------------------------------------------------------------- 

type Doc  = String
type Line = String

-------------------------------------------------------------------------- 
--	The function is a composition of the following components.	--
-------------------------------------------------------------------------- 

makeIndex
  = splitup 	>.>	--   Doc            -> [Line]
    numLines 	>.>	--   [Line]         -> [(Int,Line)] 
    allNumWords >.>	--   [(Int,Line)]   -> [(Int,Word)]
    sortLs 	>.>	--   [(Int,Word)]   -> [(Int,Word)]
    makeLists 	>.>	--   [(Int,Word)]   -> [([Int],Word)]
    amalgamate 	>.>	--   [([Int],Word)] -> [([Int],Word)]
    shorten		--   [([Int],Word)] -> [([Int],Word)]

-------------------------------------------------------------------------- 
--	Split a document into a list of lines.				--
--									--
--	Its definition is an exercise.					--
-------------------------------------------------------------------------- 

splitup :: Doc -> [Line]

splitup = splitup		-- dummy definition

-------------------------------------------------------------------------- 
--	Pair each line with its line number.				--
-------------------------------------------------------------------------- 

numLines :: [Line] -> [ ( Int , Line ) ]

numLines linels
  = zip [1 .. length linels] linels

-------------------------------------------------------------------------- 
--	Split the line component into words.				--
-------------------------------------------------------------------------- 

numWords :: ( Int , Line ) -> [ ( Int , Word ) ]

numWords (number , lin)
  = map addLineNo (splitWords lin)
    where
    addLineNo wd = (number,wd)

-------------------------------------------------------------------------- 
--	Include the definition of splitWords				--
--	Only load this file after loading the version of Section4-7.g   --
--	in this directory.						--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Split every line component into words, and join the results.	--
-------------------------------------------------------------------------- 

allNumWords :: [ ( Int , Line ) ] -> [ ( Int , Word ) ]

allNumWords = concat . map numWords

-------------------------------------------------------------------------- 
--	Example text.							--
-------------------------------------------------------------------------- 

exampleText = "cat dog\nbat dog\ncat"

-------------------------------------------------------------------------- 
--	Comparing word , line number pairs.				--
--	First look at the word ordering; two pairs with the same	--
--	word component have their numeric parts compared.		--
-------------------------------------------------------------------------- 

compare :: ( Int , Word ) -> ( Int , Word ) -> Bool
compare ( n1 , w1 ) ( n2 , w2 )
  = w1 < w2 || ( w1 == w2 && n1 < n2 )

-------------------------------------------------------------------------- 
--	Sort the list of word , line number pairs using the ordering	--
--	given by compare.						--
-------------------------------------------------------------------------- 

sortLs :: [ ( Int , Word ) ] -> [ ( Int , Word ) ]

sortLs []    = []
sortLs (a:x)
  = sortLs smaller ++ [a] ++ sortLs larger
    where
    smaller = [ b | b<-x , compare b a ]
    larger  = [ b | b<-x , compare a b ]

-------------------------------------------------------------------------- 
--	Make the number part of a pair into a list with one element.	--
-------------------------------------------------------------------------- 

makeLists ::  [ (Int,Word) ] -> [ ([Int],Word) ]

makeLists 
  = map mklis 
    where
    mklis ( n , st ) = ( [n] , st )

-------------------------------------------------------------------------- 
--	Amalgamate entries containing the same word together.		--
-------------------------------------------------------------------------- 

amalgamate :: [ ([Int],Word) ] -> [ ([Int],Word) ]

amalgamate [] = []
amalgamate [a] = [a]
amalgamate ((l1,w1):(l2,w2):rest)
  | (w1 /= w2) 	= (l1,w1) : amalgamate ((l2,w2):rest)
  | otherwise 	= amalgamate ((l1++l2,w1):rest)

examAmalgamate = amalgamate [ ([2],"bat") , ([1],"cat") , ([3],"cat") ]

-------------------------------------------------------------------------- 
--	Remove words of less than five letters.				--
-------------------------------------------------------------------------- 

shorten :: [ ([Int],Word) ] -> [ ([Int],Word) ]

shorten = filter sizer 
          where
          sizer (nl,wd) = length wd > 4

-------------------------------------------------------------------------- 
--	Erroneous definition of amalgamate.				--
-------------------------------------------------------------------------- 

amalgamate' ((l1,w1):(l2,w2):rest)
  | (w1 /= w2) 
    = (l1,w1) : amalgamate' ((l2,w2):rest)
  | otherwise 
    = (l1++l2,w1) : amalgamate' rest

-------------------------------------------------------------------------- 
--	Type declaration of exercise function.				--
-------------------------------------------------------------------------- 

-- printIndex :: [ ([Int],Word) ] -> string




--------------------------------------------------------------------------
--------------------------------------------------------------------------
--	From Section 4.7.....						--
--------------------------------------------------------------------------
--------------------------------------------------------------------------



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
  | otherwise           = a : getWord x

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

-------------------------------------------------------------------------- 
--	Getting a line of a particular length from a list of words.	--
-------------------------------------------------------------------------- 

getLine :: Int -> [Word] -> [Word]

getLine len []     = []

getLine len (w:ws)
  | length w <= len 	= w : restOfLine
  | otherwise 		= []
      where
      newlen      = len - ( length w + 1)
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

splitLines :: [Word] -> [[Word]]

splitLines [] = []
splitLines x
  = getLine lineLen x
         : splitLines (dropLine lineLen x)

-------------------------------------------------------------------------- 
--	To fill a text, split it into words, and then into lines.	--
-------------------------------------------------------------------------- 

fill :: String -> [[Word]]

fill st = splitLines (splitWords st)

