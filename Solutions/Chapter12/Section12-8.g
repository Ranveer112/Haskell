--------------------------------------------------------------------------
--                                                                      --
--	Section 12.8: Search Trees					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--                                                                      --
--	The error type.							--
--                                                                      --
-------------------------------------------------------------------------- 

data Err t = OK t | Error

val :: Err t -> t

val (OK a) = a
val Error  = error "taking value of Error element"

-------------------------------------------------------------------------- 
--	The implementation type -- hence the 'i'.			--
-------------------------------------------------------------------------- 

data Itree t = Nil | Node t (Itree t) (Itree t)

-------------------------------------------------------------------------- 
--	The abstype							--
-------------------------------------------------------------------------- 

type 
  Tree t = Itree t
  in
  nil      , -- :: Tree t
  isNil    , -- :: Tree t -> Bool
  isNode   , -- :: Tree t -> Bool
  leftSub  , -- :: Tree t -> Tree t
  rightSub , -- :: Tree t -> Tree t
  treeVal  , -- :: Tree t -> t
  insTree  , -- :: t -> Tree t -> Tree t
  delete   , -- :: t -> Tree t -> Tree t
  join     , -- :: Tree t -> Tree t -> Tree t
  minTree    -- :: Tree t -> Err t

-------------------------------------------------------------------------- 
--	Implementing the functions.					--
-------------------------------------------------------------------------- 

nil :: Tree t

nil = Nil

isNil :: Tree t -> Bool
isNil Nil = True
isNil _   = False

isNode :: Tree t -> Bool
isNode Nil = False 
isNode _   = True

leftSub :: Tree t -> Tree t
rightSub :: Tree t -> Tree t

leftSub Nil            = error "leftSub"
leftSub (Node _ t1 _) = t1

rightSub Nil            = error "rightSub"
rightSub (Node v t1 t2) = t2

treeVal  :: Tree t -> t

treeVal Nil            = error "treeVal"
treeVal (Node v _ _) = v

insTree :: Ord t => t -> Tree t -> Tree t

insTree val Nil = (Node val Nil Nil)

insTree val (Node v t1 t2)
  | v==val 	= Node v t1 t2
  | val > v 	= Node v t1 (insTree val t2)	
  | val < v 	= Node v (insTree val t1) t2	

delete :: Ord t => t -> Tree t -> Tree t

delete val (Node v t1 t2)
  | val < v 	= Node v (delete val t1) t2
  | val > v 	= Node v t1 (delete val t2)
  | isNil t2 	= t1
  | isNil t1 	= t2
  | otherwise 	= join t1 t2

join :: Ord t => Tree t -> Tree t -> Tree t

join t1 t2 
  = Node mini t1 newt
    where
    (OK mini) = minTree t2
    newt      = delete mini t2

minTree :: Ord t => Tree t -> Err t

minTree t
  | isNil t 	= Error
  | isNil t1 	= OK v
  | otherwise 	= minTree t1
      where
      t1 = leftSub t
      v  = treeVal t

-------------------------------------------------------------------------- 
--	The size function -- definable using the operations of the	--
-- 	abstype.							--
-------------------------------------------------------------------------- 

size :: Tree t -> Int
size t 
  | isNil t 	= 0
  | otherwise 	= 1 + size (leftSub t) + size (rightSub t)

-------------------------------------------------------------------------- 
--	Finding the nth element of a tree.				--
-------------------------------------------------------------------------- 

indexT :: Int -> Tree t -> t

indexT n t 
  | isNil t 	= error "indexT"
  | n < st1 	= indexT n t1
  | n == st1 	= v
  | otherwise 	= indexT (n-st1-1) t2
      where
      v   = treeVal t
      t1  = leftSub t
      t2  = rightSub t
      st1 = size t1

