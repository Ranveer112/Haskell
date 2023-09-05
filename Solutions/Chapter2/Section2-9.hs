--------------------------------------------------------------------------
--                                                                      --
--	Section 2.9: Programming with numbers and strings		--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

sales :: Int -> Int

sales 0 = 12
sales 1 = 14
sales 2 = 15
sales n = n+3

--------------------------------------------------------------------------
--	Printing a table: the top-level function.			--
--------------------------------------------------------------------------

printTable :: Int -> String

printTable n
  = heading ++ printWeeks n ++ printTotal n ++ printAverage n

-------------------------------------------------------------------------- 
--	The heading							--
-------------------------------------------------------------------------- 

heading :: String

heading = "       Week      Sales\n"

-------------------------------------------------------------------------- 
--	Print the entries for the week, assuming we have		--
--		printWeek :: Int -> String				--
-------------------------------------------------------------------------- 

printWeeks :: Int -> String

printWeeks 0 = printWeek 0
printWeeks n = printWeeks (n-1) ++ printWeek n

-------------------------------------------------------------------------- 
--	Working bottom up, use the functions				--
--		shownum	  :: Int -> String				--
--		showfloat :: Int -> Int -> String			--
--	and the justification functions ljustify, cjustify, rjustify.	--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Printing one week's entry.					--
-------------------------------------------------------------------------- 

printWeek :: Int -> String

printWeek n 
  = rPrintInt n ++ rPrintInt (sales n) ++ "\n"

offset :: Int
offset = 10

-------------------------------------------------------------------------- 
--	Numbers in the appropriate format.				--
-------------------------------------------------------------------------- 

rPrintInt :: Int -> String

rPrintInt n = rJustify offset (show n)

rJustify n st = rJustify n st		-- dummy definition

spaces :: Int -> String

spaces = spaces				-- dummy definition

-------------------------------------------------------------------------- 
--	Print the average.						--
-------------------------------------------------------------------------- 

printAverage :: Int -> String

printAverage n 
  = "\n   Average" ++ rPrintFloat (averageSales n)

-------------------------------------------------------------------------- 
--	Exercises for the reader					--
--	Complete these definitions!!					--
-------------------------------------------------------------------------- 

averageSales :: Int -> Float

averageSales = averageSales		-- dummy definition

printTotal   :: Int -> String
 
printTotal = printTotal			-- dummy definition

rPrintFloat  :: Float -> String

rPrintFloat = rPrintFloat		-- dummy definition

