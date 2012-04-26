module C_L where
{-	import qualified Text.ParserCombinators.Parsec.Token as PTok
	import qualified Text.ParserCombinators.Parsec.Language as PLan
	import qualified Text.ParserCombinators.Parsec.Char as PChar
	import qualified Text.ParserCombinators.Parsec.Combinator as PComb
	import qualified Text.ParserCombinators.Parsec.Expr as PExpr
	import qualified Text.ParserCombinators.Parsec.Prim as PPrim
	import qualified Text.ParserCombinators.Parsec.Error as PErr -}
	
	import Text.ParserCombinators.Parsec.Token  
	import Text.ParserCombinators.Parsec.Language  
	import Text.ParserCombinators.Parsec.Char  
	import Text.ParserCombinators.Parsec.Combinator  
	import Text.ParserCombinators.Parsec.Expr  
	import Text.ParserCombinators.Parsec.Prim  
	import Text.ParserCombinators.Parsec.Error   
	import List(deleteBy)
	import Maybe(fromMaybe)
	import Data.Char

-- Begin Abstract Core Language Syntax Tree -- 
	data Expr a = EVar Name 			            -- Variables
		| ENum Int 									            -- Numbers
		|	EContr Int Int					              -- Contructor tag arity
		| EAp (Expr a) (Expr a)                 -- Applications
	  | ECase (Expr a) [Alter a] 		          -- Case expressions Alternatives
		| ELet IsRec [(a, Expr a)] (Expr a)     -- Let (rec) expressions
	  | ELam [a] (Expr a) 						        -- Lambda abstractions
		
	instance Show a => Show (Expr a) where
		show (EVar x)     				= "EVar " ++ show x 
		show (ENum n)     				= "ENum " ++ show n
		show (EContr x y)   			= "EContr " ++ show x ++ " " 		++ show y
		show (EAp e e') 				= "Eap ( "  ++ show e ++ ") (" 		++ show e' ++ ")"
		show (ECase e a@(h:as)) 		= "ECase ( " ++ show e ++ ") " 		++ show a
		show (ELet i [] e)				= "ELet " ++ show i  ++ " [] " 		++ show e
		show (ELet i defs@((a,b):t) e)	= "ELet " ++ show i  ++ show defs 	++ show e
		show (ELam [] e)				= "ELam " ++ " [] "  ++ show e
		show (ELam a@(h:as) e)			= "ELam " ++ show as ++ show e
		
	type Name = String 
-- Is Recursive?
	type IsRec = Bool
	recursive, nonRecursive :: IsRec
	recursive = True
	nonRecursive = False
-- Expression
	type CoreExpr = Expr Name
	isAtomicExpr :: Expr a -> Bool
	isAtomicExpr (EVar v) = True
	isAtomicExpr (ENum n) = True
	isAtomicExpr e        = False
-- Binders of definitions
	bindersOf :: [(a,b)] -> [a]
	bindersOf defns = [name | (name, rhs) <- defns]
-- Right hand sides of
	rhssOf :: [(a,b)] -> [b]
	rhssOf defns = [rhs | (name, rhs) <- defns]
-- Alternative
	type Alter a = (Int , [a], Expr a) 
	type CoreAlt = Alter Name 
-- Program Definition
	type Program a = [ScDefn a]
	type CoreProgram = Program Name
-- Super Combinator Definiton
	type ScDefn a = (Name, [a], Expr a)
	type CoreScDefn = ScDefn Name
-- End Abstract Core Language Syntax Tree -- 

-- Begin Prelude Definitions --
	preludeDefs :: CoreProgram
	preludeDefs = [
		("I"			,["x"]				, EVar "x"),
		("K"			,["x","y"]		, EVar "x"),
		("K1"			,["x","y"]		, EVar "y"),
		("S"			,["f","g","x"], EAp (EAp (EVar "f")(EVar "x")) (EAp (EVar "g")(EVar "x"))),
		("compose",["f","g","x"], EAp (EVar "f")(EAp (EVar "g") (EVar "x"))),
		("twice"	,["f"]				, EAp (EAp (EVar "compose")(EVar "f"))(EVar "f"))
		]