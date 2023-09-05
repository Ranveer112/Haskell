--------------------------------------------------------------------------
--                                                                      --
--	Section 4.7: Supermarket billing				--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Types of the basic components of the system.			--
-------------------------------------------------------------------------- 

type Name    = String
type Price   = Int
type BarCode = Int

-------------------------------------------------------------------------- 
--	The database storing information about names and prices		--
--	is of this type:						--
-------------------------------------------------------------------------- 

type Database = [ (BarCode,Name,Price) ]

-------------------------------------------------------------------------- 
--	Example database.						--
-------------------------------------------------------------------------- 

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers" , 121),
              (5643, "Nappies" , 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]

-------------------------------------------------------------------------- 
--	The types of till inputs, and of the converted information,	--
--	names and prices replacing bar codes.				--
-------------------------------------------------------------------------- 

type TillType = [BarCode]
type BillType = [(Name,Price)]

-------------------------------------------------------------------------- 
--	Principal functions of the system.				--
-------------------------------------------------------------------------- 

-- makeBill   :: TillType -> BillType

makeBill = makeBill		-- dummy definition

-- formatBill :: BillType -> String

formatBill = formatBill		-- dummy definition

printBill  :: TillType -> String

printBill tt = formatBill (makeBill tt)

-------------------------------------------------------------------------- 
--	The length of a line of the bill, defined as a constant.	--
-------------------------------------------------------------------------- 

lineLength :: Int
lineLength = 30

-------------------------------------------------------------------------- 
--	Type declarations of formatting functions from the exercises.	--
-------------------------------------------------------------------------- 

-- formatPence :: Int -> String
-- formatLine  :: (Name,Price) -> String
-- formatLines :: [ (Name,Price) ] -> String
-- makeTotal :: BillType -> Int
-- formatTotal :: Int -> String

-------------------------------------------------------------------------- 
--	Type declarations of database functions from the exercises.	--
-------------------------------------------------------------------------- 

-- look :: Database -> BarCode -> (Name,Price)
-- lookup :: BarCode -> (Name,Price)

-------------------------------------------------------------------------- 
--	Type declarations of extension functions from the exercises.	--
-------------------------------------------------------------------------- 

-- makeDiscount :: BillType -> Int
-- formatDiscount :: Int -> String

