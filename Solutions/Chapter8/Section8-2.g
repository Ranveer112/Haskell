--------------------------------------------------------------------------
--                                                                      --
--	Section 8.2: Signatures and Instances				--
--                                                                      --
--		Gofer version						--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	Forward composition is used in this file.			--
--------------------------------------------------------------------------

infixl 9 >.>

f >.> g = g.f

--------------------------------------------------------------------------
--	The class of Visible types					--
--------------------------------------------------------------------------

class Visible t where
  toString :: t -> String
  size	   :: t -> Int

--------------------------------------------------------------------------
--	Instances of the Visible class					--
--------------------------------------------------------------------------

instance Visible Char where
  toString ch  = [ch]
  size _       = 1

instance Visible Bool where
  toString True  = "True"
  toString False = "False"
  size b = 1

instance Visible t => Visible [t] where
  toString = map toString >.> concat
  size     = map size     >.> foldr (+) 1 

--------------------------------------------------------------------------
--	Sorting ...							--
--------------------------------------------------------------------------

iSort :: Ord t => [t] -> [t]

iSort = foldr ins []

ins :: Ord t => t -> [t] -> [t]

ins a [] = [a]
ins a (b:x)
  | a<=b	= a:b:x
  | otherwise	= b : ins a x

--------------------------------------------------------------------------
--	... and printing the results.					--
--------------------------------------------------------------------------

vSort :: (Ord t,Visible [t]) => [t] -> String

vSort = iSort >.> toString

--------------------------------------------------------------------------
--	Lookup and printing						--
--------------------------------------------------------------------------

vLookupFirst :: (Eq t,Visible [u]) => [(t,u)] -> t -> String

vLookupFirst db a = toString (lookupFirst db a)

lookupFirst :: Eq t => [(t,u)] -> t -> [u]

lookupFirst l v = [ b | (a,b) <- l , a==v ]

--------------------------------------------------------------------------
--	Note that the Gofer type contexts differ from those of Haskell	--
--									--
--	In particular, if we wish to print a list of t, then we need	--
--	to state precisely that in the context. In Haskell we can only	--
--	put constraints on variables, and deduce the visibility of a	--
--	list of t from the visibility of t.				--
--                                                                      --
--	For further information about this see Section 8.4, subsection	--
--		Haskell and Gofer classes.				--
--------------------------------------------------------------------------
