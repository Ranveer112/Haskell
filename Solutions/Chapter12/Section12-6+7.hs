--------------------------------------------------------------------------
--                                                                      --
--	Sections 12.6,12.7: Simulation.					--
--	Haskell version							--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

module Simulation where

--------------------------------------------------------------------------
--                                                                      --
--      Types used in simulations.                                      --
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--      Four synonyms for numbers, used to make type declarations       --
--      more readable.                                                  --
--------------------------------------------------------------------------

type Arrival = Int
type Service = Int
type Time    = Int
type Wait    = Int

--------------------------------------------------------------------------
--      Objects arrive at most one per instant. The object is therefore ---
--      identified by its arrival time. An object brings with it its    --
--      service time -- a positive integer.                             --
--      These events are coded as objects of type inmess.               --
--------------------------------------------------------------------------

data Inmess = No | Yes Arrival Service

instance Eq Inmess where
  No == No                   = True
  Yes a1 s1 == Yes a2 s2     = (a1==a2) && (s1==s2)
  _ == _ 		     = False

--------------------------------------------------------------------------
--      A message is generated by the completion of service for a       --
--      particular object.                                              --
--      Information is provided about the arrival, waiting and service  --
--      times.                                                          --
--------------------------------------------------------------------------

data Outmess = None | Discharge Arrival Wait Service

--------------------------------------------------------------------------
--      The state of a queue is described by a list of inmessages,      --
--      together with a time (service) devoted to the first element in  --
--      the queue, and the time.                                        --
--------------------------------------------------------------------------

type
  QueueState = (Time,Service,[Inmess])
  in
  addMessage, 	-- :: Inmess -> QueueState -> QueueState
  queueStep,	-- :: QueueState -> ( QueueState , [outmess] )
  queueStart,	-- :: QueueState
  queueLength,	-- :: QueueState -> Int
  queueEmpty    -- :: QueueState -> Bool

-------------------------------------------------------------------------- 
--	The signature for the server.					--
-------------------------------------------------------------------------- 

type
 ServerState = [QueueState]
 in
 addToQueue,     -- :: Int -> Inmess -> ServerState -> ServerState
 serverStep,     -- :: ServerState -> ( ServerState , [Outmess] )
 simulationStep, -- :: ServerState -> Inmess -> ( ServerState , [Outmess] ) 
 serverStart,    -- :: ServerState
 serverSize,     -- :: ServerState -> Int
 shortestQueue   -- :: ServerState -> Int

-------------------------------------------------------------------------- 
--	Implementing the simulation					--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	The queue							--
-------------------------------------------------------------------------- 

addMessage  :: Inmess -> QueueState -> QueueState

addMessage im (time,serv,ml) = (time,serv,ml++[im])

queueStep   :: QueueState -> ( QueueState , [Outmess] )

queueStep (time , servSoFar , Yes a serv : inRest)
  | servSoFar < serv
    = ((time+1, servSoFar+1 , Yes a serv : inRest) , [])
  | otherwise
    = ((time+1, 0 , inRest) , [Discharge a (time-serv-a) serv])

queueStep (time,serv,[]) = ((time+1,serv,[]) , [])

queueStart  :: QueueState

queueStart  =  (0,0,[])

queueLength :: QueueState -> Int

queueLength (time,serv,l) = length l

queueEmpty  :: QueueState -> Bool

queueEmpty (t,s,q)  = (q==[])

-------------------------------------------------------------------------- 
--	Implementing the server						--
-------------------------------------------------------------------------- 

addToQueue :: Int -> Inmess -> ServerState -> ServerState

addToQueue n im st
  = take n st ++ [newQueueState] ++ drop (n+1) st
    where
    newQueueState = addMessage im (st!!n)

serverStep :: ServerState -> ( ServerState , [Outmess] )

serverStep [] = ([],[])
serverStep (q:qs) 
  = (q':qs' , mess++messes)
    where
    (q' , mess)    = queueStep  q
    (qs' , messes) = serverStep qs

simulationStep :: ServerState -> Inmess -> ( ServerState , [Outmess] )

simulationStep servSt im 
  = (addNewObject im servSt1 , outmess)
    where
    (servSt1 , outmess) = serverStep servSt

addNewObject :: Inmess -> ServerState -> ServerState

addNewObject No servSt = servSt

addNewObject (Yes arr wait) servSt
  = addToQueue (shortestQueue servSt) (Yes arr wait) servSt
 
serverStart :: ServerState

serverStart = copy numQueues queueStart 

serverSize :: ServerState -> Int

serverSize = length

shortestQueue :: ServerState -> Int

shortestQueue [q] = 0
shortestQueue (q:qs) 
  | (queueLength (qs!!short) <= queueLength q) 	= short+1
  | otherwise 					= 0
      where
      short = shortestQueue qs

-------------------------------------------------------------------------- 
--	numQueues is a constant to be defined.				--
-------------------------------------------------------------------------- 

numQueues :: Int
 
numQueues = 4

-------------------------------------------------------------------------- 
--	Examples							--
-------------------------------------------------------------------------- 

--	exam1 = queueStep (12,3,[Yes 8 4])
--	exam2 = queueStep (13,4,[Yes 8 4])
--	exam3 = queueStep (14,0,[])

--	serverSt1 = [ (13,4,[Yes 8 4]) , (13,3,[Yes 8 4]) ]

--	exam4 = serverStep serverSt1
--	exam5 = simulationStep (Yes 13 10) serverSt1

--------------------------------------------------------------------------
--	The copy function						--
--------------------------------------------------------------------------

copy :: Int -> t -> [t]

copy n x
  | n==0	= []
  | n>0		= x : copy (n-1) x
  | otherwise	= error "copy"

