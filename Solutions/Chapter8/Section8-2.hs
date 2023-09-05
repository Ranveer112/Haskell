--------------------------------------------------------------------------
--                                                                      --
--	Section 8.2: Signatures and Instances				--
--                                                                      --
--		Haskell version						--
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

vSort :: (Ord t,Visible t) => [t] -> String

vSort = iSort >.> toString

--------------------------------------------------------------------------
--	Lookup and printing						--
--------------------------------------------------------------------------

vLookupFirst :: (Eq t,Visible u) => [(t,u)] -> t -> String

vLookupFirst db a = toString (lookupFirst db a)

lookupFirst :: Eq t => [(t,u)] -> t -> [u]

lookupFirst l v = [ b | (a,b) <- l , a==v ]

--------------------------------------------------------------------------
--	Multiple inheritance						--
--------------------------------------------------------------------------

class (Ord t,Visible t) => OrdVis t

--------------------------------------------------------------------------
--	The following type declaration can replace that for 		--
--	vLookupFirst given above.					--
--									--
-- 		vSort :: OrdVis t => [t] -> String			--
--									--
--------------------------------------------------------------------------

