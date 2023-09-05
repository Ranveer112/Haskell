--------------------------------------------------------------------------
--                                                                      --
--	Section 10.3: Polymorphic algebraic types			--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	To use this in Haskell 1.3, the occurrences of the class Text	--
--	need to be replaced by Show, Read or both these classes.	--
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	A pair of elements of the same type.				--
-------------------------------------------------------------------------- 

data Pairs t = Pair t t 
	       deriving (Eq,Ord,Text)

exam1 :: Pairs Int
exam1 = Pair 2 3    
exam2 :: Pairs [Int]
exam2 = Pair [] [3]
exam3 = Pair [] []

-------------------------------------------------------------------------- 
--	Are the two halves of the pair equal?				--
-------------------------------------------------------------------------- 

equalPair :: Eq t => Pairs t -> Bool
equalPair (Pair x y) = (x==y)

-------------------------------------------------------------------------- 
--	Do-it-yourself lists.						--
-------------------------------------------------------------------------- 

data List t = NilList | Cons t (List t)
	      deriving (Eq,Ord,Text)

-------------------------------------------------------------------------- 
--	Example: Binary trees						--
-------------------------------------------------------------------------- 

data Tree t = Nil | Node t (Tree t) (Tree t)
	      deriving (Eq,Ord,Text)

-------------------------------------------------------------------------- 
--	The depth of a tree.						--
-------------------------------------------------------------------------- 

depth :: Tree t -> Int

depth Nil            = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

-------------------------------------------------------------------------- 
--	Collapsing a tree to a list.					--
-------------------------------------------------------------------------- 

collapse :: Tree t -> [t]
collapse Nil = []
collapse (Node v t1 t2)
  = collapse t1 ++ [v] ++ collapse t2

exam4 :: [Int]
exam4 =
      collapse (Node 12 
               (Node 34 Nil Nil) 
               (Node 3 (Node 17 Nil Nil) Nil))

-------------------------------------------------------------------------- 
--	Apply a function to each element of a tree.			--
-------------------------------------------------------------------------- 

mapTree :: (t -> u) -> Tree t -> Tree u

mapTree f Nil = Nil
mapTree f (Node v t1 t2)
  = Node (f v) (mapTree f t1) (mapTree f t2)

-------------------------------------------------------------------------- 
--	Example: union type						--
-------------------------------------------------------------------------- 

data Union t u = One t | Two u
	         deriving (Eq,Ord,Text)

exam5 = One "Duke of Prunes" 
exam6 :: Union t Float
exam6 = Two 333.12345       

-------------------------------------------------------------------------- 
--	Is an element in the first half of the union?			--
-------------------------------------------------------------------------- 

isOne :: Union t u -> Bool

isOne (One _) = True
isOne (Two _) = False

-------------------------------------------------------------------------- 
--	Joining functions to work over a union.				--
-------------------------------------------------------------------------- 

joinFuns :: (t -> v) -> (u -> v) -> Union t u -> v

joinFuns f g (One x) = f x
joinFuns f g (Two y) = g y

-------------------------------------------------------------------------- 
--	Applying a function applicable to t to an element of type	--
--		Union t u						--
-------------------------------------------------------------------------- 

applyOne :: (t -> v) -> Union t u -> v

applyOne f (One x) = f x
applyOne f (Two _) = error "applyOne applied to Two!!"

-------------------------------------------------------------------------- 
--	Exercise functions...						--
-------------------------------------------------------------------------- 

-- jFuns :: (t -> u) -> (v -> w) -> Union t v -> Union u w

data GTree t = Leaf t | Gnode [GTree t]
	       deriving (Eq,Ord,Text)

