module PrettyPrinter where
	import AST
	import Utils

-- Begin Abstract Parser Data Tree ---------------------------------------------------------------- 
  --( An abstract data type for pretty printing )
	data Iseq = INil
		| IStr String
		| IAppend Iseq Iseq
		| IIndent Iseq
		| INewline deriving Show
-- End Abstract Parser Data Tree ------------------------------------------------------------------

-- Begin Abstract Parser methods ------------------------------------------------------------------
	-- iNil
	iNil :: Iseq
	iNil = INil

	-- iStr
	iStr :: String -> Iseq
	iStr str = IStr str
		
	-- iAppend
	iAppend :: Iseq -> Iseq -> Iseq
	iAppend INil expr = expr
	iAppend expr INil   
		= expr
	iAppend seq1 seq2	= IAppend seq1 seq2
		
	-- iIndent
	iIndent	:: Iseq -> Iseq
	iIndent seq = IIndent seq

	-- iNewline
	iNewline :: Iseq
	iNewline = INewline

	-- iDisplay
	iDisplay :: Iseq -> String
	iDisplay seq = flatten 0 [(seq, 0)]

	{- 
		To make the definitions more ligible, we have used two new functions, iConcat and iInterleave.
	-}

	-- iConcat
	-- iConcat takes a list of iseqs and uses iAppend to concatenate them into a single iseq. 
	iConcat :: [Iseq] 		-> Iseq
	iConcat []	 			= iNil
	iConcat l@(seq:seqs) 	= seq `iAppend` (iConcat seqs)
	
	-- iInterleave
	-- iInterleave is similar to iConcat except that it interleaves a specified iseq between each adjacent pair 
	iInterleave :: Iseq -> [Iseq] -> Iseq
	iInterleave specSign [] 				= iNil
	iInterleave specSign l@(seq1:[]) 		= (seq1) 
	iInterleave specSign l@(seq1:seq2:[]) 	= (seq1) `iAppend` (specSign) `iAppend` (seq2)
	iInterleave specSign l@(seq:seqs) 		= (seq) `iAppend` (specSign) `iAppend` (iInterleave specSign seqs)
	
	-- iOuterleave
	iOuterleave :: Iseq -> [Iseq] -> Iseq
	iOuterleave specSign [] 				= iStr " "
	iOuterleave specSign l@(seq1:[]) 		= iStr " " `iAppend` (seq1) `iAppend` iStr " " 
	iOuterleave specSign l@(seq:seqs) 		= iStr " " `iAppend` (seq) `iAppend` (specSign) `iAppend` (iInterleave specSign seqs) `iAppend` iStr " " 
	
	-- iNum
	iNum :: Int -> Iseq
	iNum n = iStr $ show n

	-- iFWNum
	iFWNum :: Int -> Int -> Iseq
	iFWNum width n
		= iStr (fWNum width n) 
			
	
	-- iLayn
	iLayn :: [Iseq] -> Iseq
	iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
			where
			lay_item (n, seq)
				= iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline]
-- End Abstract Parser methods --------------------------------------------------------------------

-- Begin Pretty Printer --------------------------------------------------------------------------- 	
	-- Operators
	pprAritOp :: (AritOp Name) -> Iseq
	pprAritOp op = iStr $ case op of
					Plus -> "+"
					Minus -> "-"
					Mult -> "*"
					Divide -> "/"
	
	pprBoolOp :: (BoolOp Name) -> Iseq
	pprBoolOp op = iStr $ case op of
					And -> "&&"
					Or -> "||"
	
	pprRelOp :: (RelOp Name) -> Iseq
	pprRelOp op = iStr $ case op of
					Less -> "<"
					LessEq -> "<="
					Eq -> "=="
					Neq -> "<>"
					GreaterEq -> ">="
					Greater -> ">"

	-- Atomic Expressions
	pprAExpr :: CoreAExpr -> Iseq
	pprAExpr (ENum n) = iStr $ show n
	pprAExpr (EVar x) = iStr $ x
	pprAExpr (EConstr x y) = iStr ("Pack {") `iAppend` (iStr $ show x) `iAppend` (iStr $ show y)
	pprAExpr (EInnerExpr ex) = (iStr "(") `iAppend` (pprExpr ex) `iAppend` (  	iStr ")" )

	-- Expressions
	pprExpr :: CoreExpr -> Iseq
	pprExpr (EAEx a) = pprAExpr a
	pprExpr (BiExpr b) = case b of
					AritEx 	(op, e1, e2) -> pprExpr e1 `iAppend` (iStr " ") `iAppend` (pprAritOp op) `iAppend` (iStr " ") `iAppend` pprExpr e2
					BoolEx 	(op, e1, e2) -> pprExpr e1 `iAppend` (iStr " ") `iAppend` (pprBoolOp op) `iAppend` (iStr " ") `iAppend` pprExpr e2
					RelEx 	(op, e1, e2) -> pprExpr e1 `iAppend` (iStr " ") `iAppend` (pprRelOp op)  `iAppend` (iStr " ") `iAppend` pprExpr e2
									
	pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprExpr e2)
	pprExpr (ELet defns expr)
		= iConcat [ iStr "let", iNewline,
								pprDefns defns, iNewline,
								iStr "in "  , pprExpr expr ]
								
	pprExpr (ELetrec defns expr)
		= iConcat [ iStr "letrec", iNewline,
								pprDefns defns, iNewline,
								iStr "in "  , pprExpr expr ]

	-- Expression "case"
	pprExpr (ECase expr alts)
		= iConcat [iStr "case " , pprExpr expr, iStr " of", iNewline, 
							pprAlts alts]
		
	-- Expression "(\x . y)"
	pprExpr (ELam [] expr) = pprExpr expr
	pprExpr (ELam vars@(v:vs) expr) = iConcat [iStr "( \\", iInterleave  (iStr ", ") (pprVars vars),  iStr " . ", pprExpr expr , iStr " )"]

	-- Alternatives in "case" expression
	pprAlts :: [CoreAlt] -> Iseq
	pprAlts alts = iInterleave iNewline $ (map (\x -> (iIndent . pprAlt) x ) alts)
	
	-- Alternative in "case" expression	
	pprAlt :: CoreAlt -> Iseq
	pprAlt (Al (num, vars, expr)) = iIndent ( iConcat [iStr "<",  iStr $ show num, iStr ">" , iOuterleave (iStr " ") (pprVars vars ), iStr "-> ", pprExpr expr] )

	-- Pretty prints the Variables
	pprVars :: [Name] -> [Iseq]
	pprVars [] = []
	pprVars l@(str:strs) = (iStr str) : (pprVars strs)

	-- Pretty prints the definitions	
	pprDefns :: [CoreDef] -> Iseq
	pprDefns (defns)
		= iInterleave sep (map (\x -> (iIndent . pprDefn) $ x ) defns)
			where
			sep = iConcat [iStr ";" , iNewline ] 

	-- Pretty prints the definition
	pprDefn :: CoreDef -> Iseq
	pprDefn (Df defn@(name, expr) )
		= iConcat [iIndent(iStr name), iStr " = ", pprExpr expr ]
		
	-- Pretty prints the core program
	pprProgram :: CoreProgram -> Iseq
	pprProgram (Pr prg@([])) = INil 
	pprProgram (Pr prg@( (Sc (name, args, expr)) : scdfns) ) 
		= iConcat [ iInterleave (iStr " ") (iStr name : (pprVars args)), iStr " = " , pprExpr expr, iStr ";", iNewline , pprProgram $ Pr scdfns ]
		
	-- MAIN PARSE FUNCTION 
	pprint :: CoreProgram -> t -> String
	pprint prg  _ = iDisplay (pprProgram prg)


-- End Pretty Printer ----------------------------------------------------------------------------- 

	
	flatten :: Int -> [(Iseq, Int)] -> String
	flatten _ [] = ""

	flatten col ((INewline, indent) : seqs)
		= '\n' : (flatten indent seqs)

	flatten col ((IIndent seq, indent) : seqs)
		= space col ++ flatten col2 ((seq, indent) : seqs )
			where col2 = col + 2

	{- Excercise 1.6 - Add equations for flatten for IAppend, IStr and INil -}
	flatten col (((IAppend seq1 seq2), indent) : seqs)
		= (flatten col [(seq1, indent)]) ++ (flatten col [(seq2, indent)]) ++ (flatten col seqs)

	flatten col ((IStr str, indent) : seqs)
		= str ++ (flatten col seqs)

	flatten col ((INil, indent) : seqs)
		= (flatten col seqs)	
		
	-- Pretty Printes for expressions in parentheses
	mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
	mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
		where
		e2s = e2 : e2s