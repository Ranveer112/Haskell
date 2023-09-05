--------------------------------------------------------------------------
--                                                                      --
--	Section 13.5: Case study: Parsing expressions			--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

module ParsingBasics where

--------------------------------------------------------------------------
--	In Haskell1.3 and Hugs0 need to supress the definition of	--
--	fail by								--
--		import Prelude hiding (fail)				--
--------------------------------------------------------------------------

import Prelude hiding (fail)

-------------------------------------------------------------------------- 
--	Syntactic types							--
-------------------------------------------------------------------------- 

type Var = Char

data Expr = Lit Int | Var Var | Op Op Expr Expr

data Op   = Add | Sub | Mul | Div | Mod

-------------------------------------------------------------------------- 
--	The type of parsers.						--
-------------------------------------------------------------------------- 

type Parse t u = [t] -> [(u,[t])]

-------------------------------------------------------------------------- 
--	Some basic parsers						--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Fail on any input.						--
-------------------------------------------------------------------------- 

fail :: Parse t u

fail inp = []

-------------------------------------------------------------------------- 
--	Succeed, returning the value supplied.				--
-------------------------------------------------------------------------- 

succeed :: u -> Parse t u 

succeed val inp = [(val,inp)]

-------------------------------------------------------------------------- 
--	token t recognises t as the first value in the input.		--
-------------------------------------------------------------------------- 

token :: Eq t => t -> Parse t t

token t (a:x) 
  | t==a 	= [(t,x)]
  | otherwise 	= []
token t []    = []

-------------------------------------------------------------------------- 
--	spot whether an element with a particular property is the 	--
--	first element of input.						--
-------------------------------------------------------------------------- 

spot :: (t -> Bool) -> Parse t t

spot p (a:x) 
  | p a 	= [(a,x)]
  | otherwise 	= []
spot p []    = []

-------------------------------------------------------------------------- 
--	Examples.							--
-------------------------------------------------------------------------- 

bracket = token '('
dig     =  spot isDigit

isDigit :: Char -> Bool

isDigit ch = '0' <= ch && ch <= '9'

--------------------------------------------------------------------------
--	In Haskell 1.3 and Hugs0 add the definition of isDigit		--
--		isDigit ch = '0' <= ch && ch <= '9'			--
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Combining parsers						--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	alt p1 p2 recognises anything recogniseed by p1 or by p2.	--
-------------------------------------------------------------------------- 

alt :: Parse t u -> Parse t u -> Parse t u

alt p1 p2 inp = p1 inp ++ p2 inp

exam1 = (bracket `alt` dig) "234" 

-------------------------------------------------------------------------- 
--	Apply one parser then the second to the result(s) of the first.	--
-------------------------------------------------------------------------- 

infixr 5 >*>

(>*>) :: Parse t u -> Parse t v -> Parse t (u,v)
	
(>*>) p1 p2 inp 
  = [((y,z),rem2) | (y,rem1) <- p1 inp , (z,rem2)  <- p2 rem1 ]

-------------------------------------------------------------------------- 
--	Transform the results of the parses according to the function.	--
-------------------------------------------------------------------------- 

build :: Parse t u -> (u -> v) -> Parse t v

build p f inp = [ (f x,rem) | (x,rem) <- p inp ]

-------------------------------------------------------------------------- 
--	Recognise a list of objects.					--
-------------------------------------------------------------------------- 
	
list :: Parse t u -> Parse t [u]

list p = (succeed []) `alt`
         ((p >*> list p) `build` convert)
         where
         convert (a,x) = (a:x)

-------------------------------------------------------------------------- 
--	From the exercises...						--
-------------------------------------------------------------------------- 

neList   :: Parse t u -> Parse t [u]

neList = neList			--dummy definition

optional :: Parse t u -> Parse t [u]

optional = optional		--dummy definition

nTimes :: Int -> Parse t u -> Parse t [u]

nTimes = nTimes			--dummy definition

-------------------------------------------------------------------------- 
--	A parser for expressions					--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	The parser has three components, corresponding to the three	--
--	clauses in the definition of the syntactic type.		--
-------------------------------------------------------------------------- 

parser :: Parse Char Expr

parser = (litParse `alt` varParse) `alt` opExpParse

-------------------------------------------------------------------------- 
--	Spotting variables.						--
-------------------------------------------------------------------------- 

varParse :: Parse Char Expr

varParse = spot isVar `build` Var

isVar :: Char -> Bool

isVar x = ('a' <= x && x <= 'z')

-------------------------------------------------------------------------- 
--	Parsing (fully bracketed) operator applications.		--
-------------------------------------------------------------------------- 

opExpParse 
  = (token '(' >*>
     parser    >*>
     spot isOp >*>
     parser    >*>
     token ')') 
     `build` makeExpr

makeExpr (_,(e1,(bop,(e2,_)))) = Op (charToOp bop) e1 e2

isOp :: Char -> Bool

isOp = isOp		-- dummy definition

charToOp :: Char -> Op

charToOp = charToOp	-- dummy definition

-------------------------------------------------------------------------- 
--	A number is a list of digits with an optional ~ at the front. --
-------------------------------------------------------------------------- 

litParse 
  = ((optional (token '~')) >*>
     (neList (spot isDigit)))
     `build` (charlistToExpr.join) 
     where
     join (l,m) = l++m

-------------------------------------------------------------------------- 
--	From the exercises...						--
-------------------------------------------------------------------------- 

charlistToExpr :: [Char] -> Expr

charlistToExpr = charlistToExpr 	-- dummy definition

-------------------------------------------------------------------------- 
--	A grammar for unbracketed expressions.				--
--									--
--	eXpr  ::= Int | Var | (eXpr Op eXpr) |				--
--	          lexpr mop mexpr | mexpr aop eXpr			--
--	lexpr ::= Int | Var | (eXpr Op eXpr)				--
--	mexpr ::= Int | Var | (eXpr Op eXpr) |	lexpr mop mexpr		--
--	mop   ::= 'a' | '/' | '\%'					--
--	aop   ::= '+' | '-'						--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	The top-level parser						--
-------------------------------------------------------------------------- 

topLevel :: Parse t u -> [t] -> u

topLevel p inp
  = case results of
      [] -> error "parse unsuccessful"
      _  -> head results
    where
    results = [ found | (found,[]) <- p inp ]

-------------------------------------------------------------------------- 
--	The type of commands.						--
-------------------------------------------------------------------------- 

data Command = Eval Expr | Assign Var Expr | Null

-- commandParse :: Parse Char Command

-------------------------------------------------------------------------- 
--	From the exercises.						--
-------------------------------------------------------------------------- 

-- tokenList :: [t] -> Parse t [t]

-- spotWhile :: (t -> Bool) -> Parse t [t]
