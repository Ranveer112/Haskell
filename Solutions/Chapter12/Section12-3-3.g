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
  Queue t = ([t],[t])
  in
  emptyQ	:: Queue t,
  isEmptyQ	:: Queue t -> Bool,
  addQ		:: t -> Queue t -> Queue t,
  remQ		:: Queue t -> ( t , Queue t )

-------------------------------------------------------------------------- 
--	A two-list implementation of queues.				--
-------------------------------------------------------------------------- 

-- emptyQ :: Queue t

emptyQ       = ([],[])

-- isEmptyQ	:: Queue t -> Bool

isEmptyQ ([],[]) = True
isEmptyQ _       = False

-- addQ   :: t -> Queue t -> Queue t

addQ a (l,r) = (l,a:r)

-- remQ   :: Queue t -> (  t , Queue t )

remQ (a:l,r) = (a,(l,r))
remQ ([],[]) = error "remQ"
remQ ([], r) = remQ (reverse r,[])

