--------------------------------------------------------------------------
--                                                                      --
--	Section 12.3: Queues.						--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	The abstype header for queues.					--
-------------------------------------------------------------------------- 

type 
  Queue t = [t]
  in 
  emptyQ	:: Queue t,
  isEmptyQ	:: Queue t -> Bool,
  addQ		:: t -> Queue t -> Queue t,
  remQ		:: Queue t -> ( t , Queue t )

-------------------------------------------------------------------------- 
--	Queues as lists: enqueued items at the start.			--
-------------------------------------------------------------------------- 

-- emptyQ :: Queue t

emptyQ   = []

-- isEmptyQ	:: Queue t -> Bool

isEmptyQ [] = True
isEmptyQ _  = False

-- addQ   :: t -> Queue t -> Queue t

addQ a x = (a:x)

-- remQ   :: Queue t -> (  t , Queue t )

remQ x	 
  | not (isEmptyQ x) 	= (last x , init x)	
  | otherwise 		= error "remQ"		

