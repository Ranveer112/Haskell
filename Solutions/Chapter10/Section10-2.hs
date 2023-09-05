--------------------------------------------------------------------------
--                                                                      --
--	Section 10.2: Recursive Types					--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------


-------------------------------------------------------------------------- 
--	A type of arithmetic expressions.				--
-------------------------------------------------------------------------- 

data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr

-------------------------------------------------------------------------- 
--      A type of numerical trees.					--
-------------------------------------------------------------------------- 

data NTree = NilT |
             Node Int NTree NTree

-------------------------------------------------------------------------- 
--	Examples of expressions.					--
-------------------------------------------------------------------------- 

exam1 = Lit 2
exam2 = Add (Lit 2) (Lit 3)
exam3 = Add (Sub (Lit 3) (Lit 1)) (Lit 3)  

-------------------------------------------------------------------------- 
--	Expression evaluation.						--
-------------------------------------------------------------------------- 

eval :: Expr -> Int

eval (Lit n)     = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

-------------------------------------------------------------------------- 
--	Printing an expression.						--
-------------------------------------------------------------------------- 

showExpr :: Expr -> String

showExpr (Lit n) = show n
showExpr (Add e1 e2) 
  = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) 
  = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++ ")"

-------------------------------------------------------------------------- 
--	Example: trees of numbers					--
-------------------------------------------------------------------------- 

exam4 = Node 10 NilT NilT
exam5 = Node 17 (Node 14 NilT NilT) (Node 20 NilT NilT)

-------------------------------------------------------------------------- 
--	Example definitions over trees.					--
-------------------------------------------------------------------------- 

sumTree,depth :: NTree -> Int

sumTree NilT          = 0
sumTree (Node n t1 t2) = n + sumTree t1 +  sumTree t2

depth NilT            = 0
depth (Node n t1 t2)   = 1 + max (depth t1) (depth t2)

exam6 = sumTree (Node 3 (Node 4 NilT NilT) NilT) 
exam7 = depth (Node 3 (Node 4 NilT NilT) NilT) 

-------------------------------------------------------------------------- 
--	How many times does a number appear in a tree?			--
-------------------------------------------------------------------------- 

occurs :: NTree -> Int -> Int

occurs NilT p = 0
occurs (Node n t1 t2) p
  | n==p 	= 1 + occurs t1 p + occurs t2 p
  | otherwise 	=     occurs t1 p + occurs t2 p

-------------------------------------------------------------------------- 
--	Example: rearranging expressions				--
-------------------------------------------------------------------------- 

--------------------------------------------------------------------------
--	Note that the definition following immediately is commented	--
-- 	out in favour of the version using infix constructors.		--
--------------------------------------------------------------------------

{-
assoc :: Expr -> Expr

assoc (Add (Add e1 e2) e3)
  = assoc (Add e1 (Add e2 e3)) 

assoc (Add e1 e2) 
  = Add (assoc e1) (assoc e2) 
assoc (Sub e1 e2) 
  = Sub (assoc e1) (assoc e2)
assoc (Lit n) 
  = Lit n
-}

assoc :: Expr -> Expr

assoc ((e1 `Add` e2) `Add` e3)
  = assoc (e1 `Add` (e2 `Add` e3)) 

assoc (e1 `Add` e2) 
  = (assoc e1) `Add` (assoc e2) 
assoc (e1 `Sub` e2) 
  = (assoc e1) `Sub` (assoc e2)
assoc (Lit n) 
  = Lit n

--------------------------------------------------------------------------
--	Alternative definition of expressions with explicit infix	--
--	constructors.							--
--------------------------------------------------------------------------

data Nexpr = L Int | 
	     Nexpr :+: Nexpr |
	     Nexpr :-: Nexpr

-------------------------------------------------------------------------- 
--	Mutual Recursion						--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Declaring an object to be of type 				--
--		type							--
--	allows us to develop a program using the type before the 	--
--	type itself has been defined.					--
-------------------------------------------------------------------------- 

type Name = String
type Address = String

data Person = Adult Name Address Biog |
              Child Name

data Biog   = Parent String [Person] |
              NonParent String

-------------------------------------------------------------------------- 
--	Printing persons and biographies.				--
-------------------------------------------------------------------------- 

showPerson :: Person -> String

showPerson (Adult nm ad bio) 
  = showName nm ++ showAddress ad ++ showBiog bio

showBiog :: Biog -> String

showBiog (Parent st perList)
  = st ++ concat (map showPerson perList)

showName :: Name -> String

showName = id

showAddress :: Address -> String

showAddress = id

-------------------------------------------------------------------------- 
--	From the exercises...						--
-------------------------------------------------------------------------- 


-------------------------------------------------------------------------- 
--	A new definiton of expr.					--
--									--
-- 	data Expr = Lit Int |						--
--		 Op op expr expr |					--
--               If bExp expr expr					--
--									--
--	data op = Add | Sub | Mul | Div 				--
--									--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Boolean expressions.						--
-------------------------------------------------------------------------- 

data BExp = BoolLit Bool |
            And BExp BExp |
            Not BExp |
            Equal Expr Expr |
            Greater Expr Expr

bEval :: BExp -> Bool

bEval = bEval			-- dummy definition


