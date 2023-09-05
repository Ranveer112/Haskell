module Chap6 where
import Data.Char

import Prelude hiding (lookup)
maxThreeOccurs :: Int->Int->Int->(Int, Int)
maxThreeOccurs a b c = 
	(maxNum, freq)
	where
	maxNum = (max(max a b) (max b c))
	freq = length [ num | num <- [a, b, c], num==maxNum]


toString :: Int -> String
toString num | (abs num) <= 9 = (if num>=0 then "" else "-") ++ [ (intToDigit (abs num) ) ]
             | otherwise = (toString (num `div` 10)) ++ [(intToDigit (num `mod` 10)) ]


-- Supermarket excercises
type Name = String
type Price = Int
type BarCode = Int

type Database = [ (BarCode, Name, Price) ]

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers", 121), (5643, "Nappies", 1010), (3814, "Orange Jelly", 56), (1111, "Hula Hoops", 21), (1112, "Hula Hoops (Giant)", 133), (1234, "Dry Sherry, 1lt", 540)]

lineLength :: Int
lineLength = 30
type TillType = [(BarCode)]
type BillType = [(Name, Price)]


formatCents :: Price -> String
formatCents priceInCents = dollarsConvertedToChar ++ "." ++ centsConvertedToChar
		    where
		    dollars = priceInCents `div` 100
	            cents = priceInCents `mod` 100
	            dollarsConvertedToChar = toString dollars
		    centsConvertedToChar = (if cents <= 9 then "0" else "") ++ toString cents
	

formatLine :: (Name, Price) -> String
formatLine (name, priceInCents)  =  name ++ dotChars ++ priceInCentsStringified ++ "\n"
			    where
		            dotChars = replicate numDotCharsNeeded '.'
		            numDotCharsNeeded = lineLength - length name - length priceInCentsStringified
		            priceInCentsStringified = formatCents priceInCents

formatLines :: [(Name, Price)] -> String
formatLines bill | length bill == 1 = formatLine (head bill)
	         | otherwise = formatLine (head bill) ++ formatLines (tail bill)

makeTotal :: BillType -> Price
makeTotal bill = sum [ snd billLine | billLine <- bill ]


formatTotal :: Price -> String
formatTotal price = "\n" ++ prefix ++ dotChars ++ formattedPrice
	            where
		    prefix = "Total"
		    dotChars = replicate numDotCharsNeeded '.'
		    numDotCharsNeeded = lineLength - length prefix - length formattedPrice	
		    formattedPrice = formatCents price

formatHeader :: String -> String
formatHeader header = blankSpacePadLeft ++ header ++ blankSpacePadRight ++ "\n"
		      where
		      blankSpacePadRight = replicate blankSpaceNeededRight ' '
		      blankSpaceNeededRight = (lineLength - length header) `div` 2
		      blankSpacePadLeft = replicate blankSpaceNeededLeft ' '
		      blankSpaceNeededLeft = (1 + lineLength - length header) `div` 2 


formatBill :: BillType -> String
formatBill bill = (formatHeader "Haskell Store") ++ formatLines bill ++ (formatTotal (makeTotal bill)) 



look :: Database -> BarCode -> (Name, Price)
look database barcode = (matchingEntryName, matchingEntryPrice)
			where
			matchingEntries = [ (barcodeOfEntry, nameOfEntry, priceOfEntry) | (barcodeOfEntry, nameOfEntry, priceOfEntry) <- database, barcodeOfEntry == barcode ]
			(matchingEntryBarcode, matchingEntryName, matchingEntryPrice) = if length matchingEntries == 0 then (-1, "Unknown Item", 0) else head matchingEntries

lookup :: BarCode -> (Name, Price)
lookup barcode = look codeIndex barcode


makeBill :: TillType -> BillType
makeBill till = [lookup tillLine | tillLine <- till] 
-- End of supermarket excercises
