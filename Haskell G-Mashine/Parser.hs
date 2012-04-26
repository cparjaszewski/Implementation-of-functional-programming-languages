module Parser where
	import AST
	import PrettyPrinter 
	import Utils
	
	import Data.List
	import Data.Char
	import Numeric
	import qualified IO

-- Begin Parsec Modules	-----------------------------------------------------------------	
	import qualified Text.ParserCombinators.Parsec.Token as PTok
	import qualified Text.ParserCombinators.Parsec.Language as PLan
	import qualified Text.ParserCombinators.Parsec.Char as PChar
	import qualified Text.ParserCombinators.Parsec.Combinator as PComb
	import qualified Text.ParserCombinators.Parsec.Expr as PExpr
	import qualified Text.ParserCombinators.Parsec.Prim as PPrim
	import qualified Text.ParserCombinators.Parsec.Error as PErr 
-- End Parsec Modules	-----------------------------------------------------------------

-- Begin Parser -----------------------------------------------------------------------
	prog :: PPrim.GenParser Char st CoreProgram
	prog = do { PPrim.skipMany whitespace 
					; prg <- PComb.sepEndBy1 sc (do { PPrim.many whitespace ; PChar.char ';' ; PPrim.many whitespace})
					; PComb.eof
					; return (Pr prg )
					}
			PPrim.<?> "Program"

	sc :: PPrim.GenParser Char st CoreScDefn
	sc = do { vs@(x:xs) <- (PComb.sepEndBy ivar ( PPrim.many PChar.space )) 
				 ; PChar.char '='
				 ; PPrim.many whitespace
				 ; e <- expr
				 ; return $ Sc (x, xs, e)
				 }
			PPrim.<?> "Supercombinator"

	defns :: PPrim.GenParser Char st [CoreDef]
	defns = do { d1 <- def
						; ds <- PPrim.many (PPrim.try (do { PPrim.many whitespace 
															; PChar.char ';'
															; PPrim.many whitespace
															; d <- def
															; return d
															}))
						; return (d1:ds)
						}              
			PPrim.<?> "Definitions"
																	
	def :: PPrim.GenParser Char st CoreDef
	def = do { v <- ivar
					; PPrim.many PChar.space
					; PChar.char '='
					; PPrim.many PChar.space
					; ex <- expr
					; return $ Df (v, ex)
					}
			PPrim.<?> "Definition"

	expr :: PPrim.GenParser Char st (CoreExpr)
	expr =  PExpr.buildExpressionParser table (do {PPrim.many PChar.space
													; x<-nonrecexpr
													; PPrim.many PChar.space
													;  return x})
			PPrim.<|> nonrecexpr
			PPrim.<?> "expression" 

	table :: [[PExpr.Operator Char st (CoreExpr)]]
	table  = [[op ""   (\x y->(EAp x y)) PExpr.AssocLeft]
            ,[op "*"  (\x y->(BiExpr (AritEx(Mult, x, y)))) PExpr.AssocLeft, 
	          op "/"  (\x y->(BiExpr (AritEx(Divide, x, y)))) PExpr.AssocNone
						 ]
            ,[op "+"  (\x y->(BiExpr (AritEx(Plus, x, y)))) PExpr.AssocLeft, 
						  op "-"  (\x y->(BiExpr (AritEx(Minus, x, y)))) PExpr.AssocNone
						 ]
            ,[op "==" (\x y->(BiExpr (RelEx (Eq, x, y)))) PExpr.AssocNone, 
							op "~=" (\x y->(BiExpr (RelEx (Neq, x, y)))) PExpr.AssocNone, 
							op ">"  (\x y->(BiExpr (RelEx (Greater, x, y)))) PExpr.AssocNone, 
							op ">=" (\x y->(BiExpr (RelEx (GreaterEq, x, y)))) PExpr.AssocNone, 
							op "<"  (\x y->(BiExpr (RelEx (Less, x, y)))) PExpr.AssocNone, 
							op "<=" (\x y->(BiExpr (RelEx (LessEq, x, y)))) PExpr.AssocNone
							]
            ,[op "&"  (\x y->(BiExpr (BoolEx (And, x, y)))) PExpr.AssocRight]
            ,[op "|"  (\x y->(BiExpr (BoolEx (Or, x, y)))) PExpr.AssocRight]
            ]          
          where
          op s f assoc = PExpr.Infix (do{PChar.string s;  return f}) assoc

-- Non recursive expressions
	nonrecexpr :: PPrim.GenParser Char st (CoreExpr)
	nonrecexpr = do { aex <- aexpr
                   ; return (EAEx aex)
                   }
		 PPrim.<|> do { PChar.char '\\'
             ; vs <- (PComb.sepEndBy ivar (PPrim.many PChar.space)) 
             ; PChar.char '.'
             ; PPrim.many PChar.space
             ; ex <- expr
             ; return (ELam vs ex)
             }
		 PPrim.<|> do { PChar.string "case"
					 ; PPrim.many PChar.space
					 ; e <- expr
					 ; PPrim.many PChar.space
					 ; PChar.string "of"
					 ; PComb.many1 PChar.space
					 ; a <- alts
					 ; return (ECase e a)   
					 }            
		 PPrim.<|> do { PPrim.try (PChar.string "letrec") 
					 ; PComb.many1 PChar.space
					 ; d <- defns
					 ; PChar.string "in"
					 ; PComb.many1 PChar.space
					 ; e <- expr
					 ; return (ELetrec d e)
					 }
		PPrim.<|> do { PChar.string "let" 
					 ; PComb.many1 PChar.space
					 ; d <- defns
					 ; PChar.string "in"
					 ; PPrim.many PChar.space
					 ; e <- expr
					 ; return (ELet d e)
					 }   
		PPrim.<?> "NonRecursiveExpr"

	-- Alternatives
	alts :: PPrim.GenParser Char st [CoreAlt]
	alts = do { PPrim.many whitespace
					 ; a1 <- alt
					 ; as <- PPrim.many (PPrim.try (do { PPrim.many whitespace
																 ; PChar.char ';'
																 ; PPrim.many whitespace
																 ; a <- alt
																 ; return a
																 }))
					 ; return (a1:as)
					 } 
				PPrim.<?> "Alternatives"
	alt :: PPrim.GenParser Char st CoreAlt
	alt = do { PChar.char '<'
					; n <- num
					; PChar.char '>'
					; PPrim.many PChar.space
					; vars <- (PComb.sepEndBy ivar (PComb.many1 PChar.space))
					; PChar.string "->"
					; PPrim.many whitespace
					; ex <- expr          
					; PPrim.many PChar.space            
					; return $ (Al (n, vars, ex))
					} 
			PPrim.<?> "Alternative"

	-- Atomic expressions
	aexpr :: PPrim.GenParser Char st (EAExpr a)
	aexpr = PPrim.try( do { v <- var 
						;	return v
						})
		 PPrim.<|> do { n <- num
						; return (ENum n)
						}
		 PPrim.<|> do { PChar.string "Pack{"
						; a <- num
						; PChar.char ','
						; b <- num
						; PChar.char '}'
						; return (EConstr a b)
						}
		 PPrim.<|> do { PChar.string "("
						; ex <- expr 
						; PChar.string ")"
						; return (EInnerExpr ex)
						}
		PPrim.<?> "Atomic expr"

	-- Variables (Identificators)	
	ivar :: PPrim.GenParser Char st Name
	ivar = do { PPrim.many PChar.space
						; v <- PChar.letter
						; vs <- PPrim.many PChar.alphaNum
						; case checkForKeyword (v:vs) keywords of 
								Just x -> return x
								Nothing -> fail "keyword cannot be used as identificator"
						}
			 PPrim.<?> "one or more variables"
	
	-- Simple Char Variables (Indentyficastors)
	--	var :: PPrim.GenParser Char st (EVar String)
	var = do { v <- ivar
						; return (EVar v)
					}
			PPrim.<?> "atomic - identificator"    

	-- Both positive and negative numbers
	num :: PPrim.GenParser Char st Int
	num = do { mn <- PPrim.try (do {m <- PChar.char '-'
														; return m})
						; c <- posNum
						; return $ stringToInt (mn:c)
					}
				PPrim.<|> do {	c <- posNum
						; return $ stringToInt c
					} 
			PPrim.<?> "(num) - number"
			
	-- The version of positive number catcher - returns a string
	posNum :: PPrim.GenParser Char st String
	posNum = do {	c <- PChar.oneOf "123456789"
						; PComb.notFollowedBy PChar.letter
						; tail <- PPrim.try (do { cs <- PPrim.many $ PChar.oneOf "0123456789"
								 ; return cs
								 })
						; return (c:tail)
					} 
					
	-- Simple Char whitespaces
	whitespace :: PPrim.GenParser Char st Char
	whitespace = PChar.char ' '
					PPrim.<|> PChar.char '\n'  
					PPrim.<|> PChar.char '\t'
	
	whitespaces :: PPrim.GenParser Char st Name
	whitespaces = PComb.many1 whitespace   

	-- Keywords
	keywords :: [String]
	keywords = ["Pack","case","of","letrec","let","in"]
	
	checkForKeyword :: (Eq a) => a -> [a] -> Maybe a
	checkForKeyword x [] = Just x
	checkForKeyword y (x:xs) =  if x==y then Nothing
																		 else checkForKeyword y xs   
	 
	stringToInt :: String -> Int
	stringToInt str = 
	  case readDec str of
	    [(n,"")] -> n
-- End Parser   -----------------------------------------------------------------------  

-- Begin Parsing IO Function ----------------------------------------------------------
{-
	tst :: PPrim.GenParser Char () CoreProgram -> String -> (CoreProgram -> Int -> String) -> IO.Handle -> IO String
	tst p input printer fhandle = case (PPrim.parse p "" input) of
                     Left err -> return ("parse error at "++ (show err))
                     Right x  -> do { (IO.hPutStr fhandle (printer x 0))
                                    ; return "*------------------------*\n|   Parsing succesfull   |\n*------------------------*\n\n\n"
                                    }
	takename x [] = x
	takename x (y:ys) = if (y == '.') then x
                                   else (takename (x++[y]) ys)  

	
-}
{-
	parseCr = do { filename <- IO.getLine
					 ; filehandle <- (IO.openFile filename IO.ReadMode)
					 ; contents <- (IO.hGetContents filehandle)
					 ; outhandle <- (IO.openFile ((takename [] filename)++".out") IO.WriteMode)
					 ; effect <- (tst prog contents pprint outhandle)
					 ; putStrLn effect
					 ; IO.hClose filehandle
					 ; IO.hClose outhandle
					 }  
-}
	-- function that take a file name and return CoreProgram
	{-parse :: String -> CoreProgram
	parse filename = do { filehandle <- (IO.openFile filename IO.ReadMode)
								; contents <- (IO.hGetContents filehandle)
								; cp <- doParse prog contents 
								; return cp
								} -}
	{-doParse p input = case (PPrim.parse p "" input) of
													Left err -> return ("parse error at "++ (show err))
													Right x  -> return x		-}
-- End Parsing IO Function ------------------------------------------------------------