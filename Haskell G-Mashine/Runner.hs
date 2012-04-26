module Runner where
	import AST	
	import PrettyPrinter
	import Parser
	import Utils
	import Mark1
	import qualified IO
	import qualified TIM
	
-- Begin Parsec Modules	-----------------------------------------------------------------	
	import qualified Text.ParserCombinators.Parsec.Token as PTok
	import qualified Text.ParserCombinators.Parsec.Language as PLan
	import qualified Text.ParserCombinators.Parsec.Char as PChar
	import qualified Text.ParserCombinators.Parsec.Combinator as PComb
	import qualified Text.ParserCombinators.Parsec.Expr as PExpr
	import qualified Text.ParserCombinators.Parsec.Prim as PPrim
	import qualified Text.ParserCombinators.Parsec.Error as PErr 
-- End Parsec Modules	--------------------------------------------------------------------
	
-- Begin Utils ---------------------------------------------------------------------------
	takename x [] = x
	takename x (y:ys) = if (y == '.') then x
                                   else (takename (x++[y]) ys)  
	
	tstP :: (Num t) => PPrim.GenParser tok () CoreProgram -> [tok] -> (CoreProgram -> t -> String) -> IO.Handle -> IO Name
	tstP p input printer fhandle = case (PPrim.parse p "" input) of
                    Left err -> return ("parse error at " ++ (show err))
                    Right x  -> do { (IO.hPutStr fhandle (printer x 0))
                                   ; return "*------------------------*\n|   Parsing succesfull   |\n*------------------------*\n\n\n"
                                   }
																	 
	tstM1 :: (Num t) => PPrim.GenParser tok () CoreProgram -> [tok] -> (CoreProgram -> t -> String) -> IO Name
	tstM1 p input printer = case (PPrim.parse p "" input) of
                    Left err -> return ("parse error at " ++ (show err))
                    Right x  -> do { IO.hSetBuffering IO.stdin IO.LineBuffering
									; IO.putStrLn (runProg x) 
									; return ""
                                   }

	tstTIM :: (Num t) => PPrim.GenParser tok () CoreProgram -> [tok] -> (CoreProgram -> t -> String) -> IO Name
	tstTIM p input printer = case (PPrim.parse p "" input) of
                    Left err -> return ("parse error at " ++ (show err))
                    Right x  -> do { IO.hSetBuffering IO.stdin IO.LineBuffering
									; IO.putStrLn (runTIMProg x) 
									; return ""
                                   }

-- End   Utils ---------------------------------------------------------------------------	
	
-- Begin Pretty PRinter Runner  ----------------------------------------------------------

	rpt :: FilePath -> IO ()
	rpt filename = do {	filehandle <- (IO.openFile filename IO.ReadMode)
					; contents <- (IO.hGetContents filehandle)
					; outhandle <- (IO.openFile ((takename [] filename)++".out") IO.WriteMode)
					; effect <- (tstP prog contents pprint outhandle)
					; putStrLn effect
					; IO.hClose filehandle
					; IO.hClose outhandle
					}
	
	rM1t :: FilePath -> IO ()
	rM1t filename = do {	filehandle <- (IO.openFile filename IO.ReadMode)
					; contents <- (IO.hGetContents filehandle)
					; effect <- (tstM1 prog contents pprint)
					; putStrLn effect
					; IO.hClose filehandle
					}
					
	rTIMt filename = do {	filehandle <- (IO.openFile filename IO.ReadMode)
					; contents <- (IO.hGetContents filehandle)
					; effect <- (tstTIM prog contents pprint)
					; putStrLn effect
					; IO.hClose filehandle
					}

-- End   Pretty PRinter Runner  ----------------------------------------------------------

-- Begin Mark Runner  --------------------------------------------------------------------
	
	runProg :: CoreProgram -> String
	runProg p = showResults . eval . compile $ p
	
	runTIMProg :: CoreProgram -> String
	runTIMProg p = TIM.showFullResults . TIM.eval . TIM.compile $ p
	
	
	parseCr :: FilePath -> CoreProgram
	parseCr name = case PPrim.parse prog name "" of
						Left err -> Pr [Sc ("error", [], EAEx (EVar " has occured in parsing"))]
						Right p -> p

-- End  Mark Runner  --------------------------------------------------------------------