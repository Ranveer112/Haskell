--------------------------------------------------------------------------
--                                                                      --
--	Section 12.9: Case Study: sets					--
--	Gofer version							--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	The abstype signature for sets of objects of type a.		--
-------------------------------------------------------------------------- 

type
  Set t = [t]
  in
  empty              , -- :: Set t
  sing               , -- :: t -> Set t
  memSet             , -- :: Ord t => Set t -> t -> Bool
  union,inter,diff   , -- :: Ord t => Set t -> Set t -> Set t
  eqSet              , -- :: Eq t  => Set t -> Set t -> Bool
  subSet,leqSet      , -- :: Ord t => Set t -> Set t -> Bool
  makeSet            , -- :: Ord t => [t] -> Set t
  mapSet             , -- :: Ord u => (t -> u) -> Set t -> Set u
  filterSet          , -- :: (t->Bool) -> Set t -> Set t
  foldSet            , -- :: (t -> t -> t) -> t -> Set t -> t
  showSet            , -- :: (t->String) -> Set t -> String
  card               , -- :: Set t -> Int
  setLimit             -- :: Ord t => (Set t -> Set t) -> Set t -> Set t

-------------------------------------------------------------------------- 
--	The implementation.						--
--	Ordered lists without repetitions.				--
-------------------------------------------------------------------------- 

empty               :: Set t
empty  = []

sing                :: t -> Set t
sing a = [a]

memSet              :: Ord t => Set t -> t -> Bool
memSet [] b    = False
memSet (a:x) b 
  | a<b		= memSet x b
  | a==b 	= True
  | otherwise 	= False

union    :: Ord t => Set t -> Set t -> Set t
union [] y        = y
union x []        = x
union (a:x) (b:y) 
  | a<b 	= a : union x (b:y)
  | a==b 	= a : union x y
  | otherwise 	= b : union (a:x) y

inter    :: Ord t => Set t -> Set t -> Set t
inter [] y = []
inter x [] = []
inter (a:x) (b:y) 
  | a<b 	= inter x (b:y)
  | a==b 	= a : inter x y
  | otherwise 	= inter (a:x) y

diff    :: Ord t => Set t -> Set t -> Set t
diff [] y = []
diff x [] = x
diff (a:x) (b:y)  
  | a<b 	= a : diff x (b:y)
  | a==b 	= diff x y
  | otherwise 	= diff (a:x) y

subSet        :: Ord t => Set t -> Set t -> Bool
subSet [] y = True
subSet x [] = False
subSet (a:x) (b:y) 
  | a<b 	= False
  | a==b 	= subSet x y
  | a>b 	= subSet (a:x) y

eqSet        :: Eq [t] => Set t -> Set t -> Bool
--eqSet x y = subSet x y && subSet y x
eqSet = (==)

leqSet        :: Ord [t] => Set t -> Set t -> Bool
leqSet = (<=)
	
makeSet             :: Ord t => [t] -> Set t
makeSet = remDups . sort
          where
          remDups []     = []
          remDups [a]    = [a]
          remDups (a:b:x) 
	    | a < b 	= a : remDups (b:x)
            | otherwise = remDups (b:x)
	  sort = sort -- dummy definition

mapSet              :: Ord u => (t -> u) -> Set t -> Set u
mapSet f  = makeSet . (map f)

filterSet           :: (t->Bool) -> Set t -> Set t
filterSet = filter

foldSet             :: (t -> t -> t) -> t -> Set t -> t
foldSet   = foldr

showSet             :: (t->String) -> Set t -> String
showSet f = concat . (map ((++"\n") . f))

card                :: Set t -> Int
card      = length

setLimit             :: Ord [t] => (Set t -> Set t) -> Set t -> Set t
setLimit f x 
  | eqSet x next 	= x
  | otherwise 		= setLimit f next
                 where
                 next = f x

--------------------------------------------------------------------------
--	Sets can be compared for equality and order, so long as lists	--
--	of the constituent objects can.					--
--------------------------------------------------------------------------

instance Eq [t] => Eq (Set t) where
  (==) = eqSet

instance Ord [t] => Ord (Set t) where
  (<=) = leqSet

-------------------------------------------------------------------------- 
--	From the exercises....						--
-------------------------------------------------------------------------- 

-- symmDiff :: Set t -> Set t -> Set t

-- powerSet :: Set t -> Set (Set t)

-- setUnion :: Set (Set t) -> Set t
-- setInter :: Set (Set t) -> Set t

