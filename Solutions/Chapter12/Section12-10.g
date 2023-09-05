--------------------------------------------------------------------------
--                                                                      --
--	Section 12.10: Relations and Graphs.				--
--	Gofer version							--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	To be complied after Section12-9.g				--
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	A relation is a set of pairs.					--
-------------------------------------------------------------------------- 

type Relation t = Set (t,t)

-------------------------------------------------------------------------- 
--	Operations over relations.					--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	The image of an element under a relation.			--
-------------------------------------------------------------------------- 

image :: Ord t => Relation t -> t -> Set t

image rel val = mapSet snd (filterSet ((==val).fst) rel)

-------------------------------------------------------------------------- 
--	The image of a set of elements under a relation.		--
-------------------------------------------------------------------------- 

setImage :: Ord (Set t) => Relation t -> Set t -> Set t

setImage rel = unionSet . mapSet (image rel) 

-------------------------------------------------------------------------- 
--	The union of a set of sets.					--
-------------------------------------------------------------------------- 

unionSet :: Ord t => Set (Set t) -> Set t

unionSet = foldSet union empty

-------------------------------------------------------------------------- 
--	Add to a set its image under a relation.			--
-------------------------------------------------------------------------- 

addImage :: Ord (Set t) => Relation t -> Set t -> Set t

addImage rel st = st `union` setImage rel st

-------------------------------------------------------------------------- 
--	Add the children (under the relation isParent) to a set.	--
-------------------------------------------------------------------------- 

-- type People = String

-- isParent :: Relation People

-- isParent = isParent    -- dummy definition
   		          -- needs to be replaced

-- addChildren :: Set People -> Set People

-- addChildren = addImage isParent 

-------------------------------------------------------------------------- 
--	Compose two relations.						--
-------------------------------------------------------------------------- 

compose :: Ord (Set ((t,t),(t,t))) => 
	   Relation t -> Relation t -> Relation t

compose rel1 rel2
  =  mapSet outer (filterSet equals (setProduct rel1 rel2))
     where
     equals ((a,b),(c,d)) = (b==c)
     outer  ((a,b),(c,d)) = (a,d)

-------------------------------------------------------------------------- 
--	The product of two sets.					--
-------------------------------------------------------------------------- 

setProduct :: Ord (Set (t,u)) => Set t -> Set u -> Set (t,u)

setProduct st1 st2 = unionSet (mapSet (adjoin st1) st2)

-------------------------------------------------------------------------- 
--	Add an element to each element of a set, forming a set of pairs.--
-------------------------------------------------------------------------- 

adjoin :: Ord (t,u) => Set t -> u -> Set (t,u)

adjoin st el = mapSet (addEl el) st
               where
               addEl el el' = (el',el)

-------------------------------------------------------------------------- 
--	The transitive closure of a relation.				--
-------------------------------------------------------------------------- 

tClosure :: ( Ord (Set ((t,t),(t,t))) , Ord [(t,t)] ) =>
	    Relation t -> Relation t

tClosure rel = setLimit addGen rel
               where
               addGen rel' = rel' `union` compose rel' rel

-------------------------------------------------------------------------- 
--	Graphs								--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	The connected components of a graph.				--
-------------------------------------------------------------------------- 

connect :: ( Ord (Set ((t,t),(t,t))) , Ord [(t,t)] ) =>
	   Relation t -> Relation t

connect rel = clos `inter` solc
              where
              clos = tClosure rel
              solc = inverse clos

-------------------------------------------------------------------------- 
--	The inverse of a relation -- swap all pairs.			--
-------------------------------------------------------------------------- 

inverse :: Ord (t,t) => Relation t -> Relation t

inverse = mapSet swap
          where 
          swap (a,b) = (b,a)


-------------------------------------------------------------------------- 
--	The equivalence classes of a(n equivalence) relation.		--
-------------------------------------------------------------------------- 

classes :: Ord [Set t] => Relation t -> Set (Set t)

classes rel 
  = setLimit (addImages rel) start
    where
    start = mapSet sing (eles rel)

-------------------------------------------------------------------------- 
--	The auxiliary functions used in classes.			--
-------------------------------------------------------------------------- 

eles :: Ord t => Relation t -> Set t

eles rel = mapSet fst rel `union` mapSet snd rel

addImages :: Ord (Set t) => Relation t -> Set (Set t) -> Set (Set t)

addImages rel = mapSet (addImage rel)

-------------------------------------------------------------------------- 
--	Searching in graphs						--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	The types of the two graph searching algorithms			--
-------------------------------------------------------------------------- 

-- breadthFirst :: Relation t -> t -> [t]
-- depthFirst   :: Relation t -> t -> [t]

-------------------------------------------------------------------------- 
--	The descendants v under rel which lie outside st.		--
-------------------------------------------------------------------------- 

newDescs :: Ord t => Relation t -> Set t -> t -> Set t
newDescs rel st v = image rel v `diff` st

-------------------------------------------------------------------------- 
--	Breaking the abstraction barrier for sets.			--
-------------------------------------------------------------------------- 

flatten :: Set t -> [t]

flatten = flatten	-- dummy definition

-------------------------------------------------------------------------- 
--	Under the list implementation, we can use			--
--		flatten = id						--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	A list of new descendants.					--
-------------------------------------------------------------------------- 

findDescs :: Ord t => Relation t -> [t] -> t -> [t]
findDescs rel l v = flatten (newDescs rel (makeSet l) v)

-------------------------------------------------------------------------- 
--	Finding the limit of a function.				--
--									--
--	generalLimit f x is the first member of the sequence		--
--		x, f x, f (f x), ...					--
--	equal to its successor.						--
-------------------------------------------------------------------------- 

generalLimit :: Eq t => (t -> t) -> t -> t
generalLimit f x 
  | x == next	 	= x
  | otherwise 		= generalLimit f next
                   where
                   next = f x

-------------------------------------------------------------------------- 
--	Breadth first search.						--
-------------------------------------------------------------------------- 

breadthFirst :: ( Ord t , Eq [t] ) => Relation t -> t -> [t]

breadthFirst rel val
	= generalLimit step start
	  where
	  start = [val]
	  step l = l ++ nub (concat (map (findDescs rel l) l))

-------------------------------------------------------------------------- 
--	Depth first search.						--
--------------------------------------------------------------------------

depthFirst :: Ord t => Relation t -> t -> [t]

depthSearch :: Ord t => Relation t -> t -> [t] -> [t]

depthFirst rel v = depthSearch rel v []

depthSearch rel v used
	= v : depthList rel (findDescs rel used' v) used'
	  where
	  used' = v:used

depthList rel [] used = [] 

depthList rel (val:rest) used
  = next ++ depthList rel rest (used++next)
    where
    next 
      | elem val used 	 = []
      | otherwise 	 = depthSearch rel val used

-------------------------------------------------------------------------- 
--	From the exercises...						--
-------------------------------------------------------------------------- 

-- distance :: Eq t => Relation t -> t -> t -> Int



