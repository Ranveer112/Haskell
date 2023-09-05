--------------------------------------------------------------------------
--                                                                      --
--	Section 13.8: Case study: Simulation				--
--	Gofer version							--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--	Load these files first:						--
--									--
--		../Chapter12/Section12-6+7.g				--
--		Section13-6.g						--
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Do the simulation -- give a starting state and list of		--
--	Inmess to produce a list of Outmess.				--
-------------------------------------------------------------------------- 

doSimulation :: ServerState -> [Inmess] -> [Outmess]

doSimulation servSt (im:messes)
  = outmesses ++ doSimulation servStNext messes
    where
    (servStNext , outmesses) = simulationStep servSt im

-------------------------------------------------------------------------- 
--	The random sequence of times.					--
-------------------------------------------------------------------------- 

randomTimes 
  = map (makeFunction dist . fromInteger) (randomSequence seed)

-------------------------------------------------------------------------- 
--	The input sequence.						--
-------------------------------------------------------------------------- 

simulationInput = zipWith Yes [1 .. ] randomTimes 

-------------------------------------------------------------------------- 
--	Test the simulation.						--
-------------------------------------------------------------------------- 

testSimulation = doSimulation serverStart simulationInput

-------------------------------------------------------------------------- 
--	A second input sequence.					--
-------------------------------------------------------------------------- 

simulationInput2 = take 50 simulationInput ++ noes

noes = No : noes

-------------------------------------------------------------------------- 
--	 A second test.							--
-------------------------------------------------------------------------- 

testSimulation2 = take 50 (doSimulation serverStart simulationInput2)

-------------------------------------------------------------------------- 
--	Measuring the total waiting time.				--
-------------------------------------------------------------------------- 

totalWait :: [Outmess] -> Int

totalWait = sum . map waitTime
            where
            waitTime (Discharge t w s) = w


