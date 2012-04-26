module Language where
--import Utils
data Expr a
	= EVar Name 		-- zmienne
	| ENum Int 		-- liczby
	| EConstr Int Int 	-- konstruktor znacznik(tag) arnosc
	| EAp (Expr a) (Expr a) -- aplikacje
	| ELet			-- wyra¿enia let(rec)
		IsRec 		-- true, jeœli jest rekurencyjne
		[(a, Expr a)] 	-- definicje
		(Expr a) 	-- cia³o let(rec)
	| ECase 		-- wyra¿enia 'case'
		(Expr a) 	-- analizowane wyra¿enie
		[Alter a] 	-- alternatywy
	| ELam [a] (Expr a) 	-- lambda
			deriving (Show)
type CoreExpr = Expr Name

type Name = String

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

bindersOf::[(a,b)]->[a]
bindersOf defns = [name | (name, rhs)<-defns]
rhssOf::[(a,b)]->[b]
rhssOf defns =[rhs | (name, rhs)<-defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name
isAtomicExpr::Expr a->Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr _ = False

type Program a = [ScDefn a]
type CoreProgram = Program Name
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

--
-- main = silnia 10
-- silnia n = case n of
--		<0> -> 1;
--

preludeDefs :: CoreProgram
preludeDefs
	= [	("I", ["x"], EVar "x"),
		("K", ["x","y"], EVar "x"),
		("K1", ["x", "y"], EVar "y"),
		("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),
		("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
		("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))]

exRazyDwa::CoreProgram
exRazyDwa
	= [ 	("main", [], EAp (EVar "razyDwa") (ENum 4)),
		("razyDwa", ["x"], EAp (EAp (EVar "*") (ENum 2)) (EVar "x"))]

