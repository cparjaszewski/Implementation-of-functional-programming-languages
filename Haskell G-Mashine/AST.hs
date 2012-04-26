module AST where
	import List(deleteBy)
	import Maybe(fromMaybe)
	import Data.Char
	import Utils

-- Begin Primitive Types -----------------------------------------------------------------
	type IsRec = Bool
	recursive, nonRecursive :: IsRec
	recursive = True
	nonRecursive = False
-- End Primitive Types -------------------------------------------------------------------
	
-- Begin Abstract Syntax Tree ------------------------------------------------------------
	-- Operators
	data AritOp a = Plus | Minus | Mult | Divide
	data RelOp  a = Less | LessEq | Eq | Neq | GreaterEq | Greater
	data BoolOp a = And | Or

	-- Binary Operations	
	data BiEx a = AritEx (AritOp a, Expr a, Expr a)
		| BoolEx (BoolOp a, Expr a, Expr a)
		| RelEx (RelOp a, Expr a, Expr a)
	
	-- Atomic expressions 
	type CoreAExpr = EAExpr Name
	data EAExpr a = EVar Name
		| ENum Int
		| EConstr Int Int
		| EInnerExpr (Expr Name)
	
	-- Expressions
	type CoreExpr = Expr Name
	data Expr a = EAEx (EAExpr a)             	-- Atomic expressions
		| BiExpr (BiEx a)   					-- Binary operations
		| EAp (Expr a) (Expr a)                 -- Applications	
		| ECase (Expr a) [Alter a] 		        -- Case expressions Alternatives
		| ELet [Def a] (Expr a)     			-- Non-recursice Let expressions
		| ELetrec [Def a] (Expr a)     			-- Recursive Let expressions
		| ELam [a] (Expr a) 					-- Lambda abstractions	
		
	-- Definitions
	type CoreDef = Def Name
	data Def a = Df (a, Expr a)
	-- Alternative 
	type CoreAlt = Alter Name  
	data Alter a = Al (Int, [a], Expr a)
	-- Program Definition
	type CoreProgram = Program Name 
	data Program a = Pr [ScDefn a]
	fromPr :: Program  a -> [ScDefn a]
	fromPr (Pr pr) = pr
	
	-- Super Combinator Definiton
	type CoreScDefn = ScDefn Name  
	data ScDefn a = Sc (Name, [a], Expr a)
	fromSC :: ScDefn a -> (Name, [a], Expr a)
	fromSC (Sc trio) = trio
-- End  Abstract Core Language Syntax Tree ------------------------------------------------


-- Begin Prelude Definitions --------------------------------------------------------------
	preludeDefs :: CoreProgram
	preludeDefs = Pr [
		Sc ("I" ,["x"] , EAEx ( EVar "x")),
		Sc ("K"	,["x","y"] , EAEx ( EVar "x")),
		Sc ("K1",["x","y"] , EAEx ( EVar "y")),
		Sc ("S"	,["f","g","x"], EAp  ( EAp  ( EAEx ( EVar "f" ))( EAEx ( EVar "x" ))) ( EAp ( EAEx ( EVar "g" ))( EAEx ( EVar "x" )))),
		Sc ("compose" ,["f","g","x"], EAp  ( EAEx ( EVar "f" ) ) ( EAp ( EAEx ( EVar "g" )) ( EAEx ( EVar "x" )))),
		Sc ("twice"	  ,["f"] , EAp  ( EAp  ( EAEx ( EVar "compose" ))( EAEx ( EVar "f" ))) ( EAEx ( EVar "f" )))
		]
-- End Prelude Definitions ----------------------------------------------------------------