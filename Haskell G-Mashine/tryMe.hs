module Main where

   import Text.ParserCombinators.Parsec
   import qualified IO 
   import Text.ParserCombinators.Parsec.Expr
 
-- BEGIN DATA DEFS *******************************************************************************************************   

   data Variable a = Variable a
   data Number a = Number a
   data BinaryOp a b c = Arithmetic_op a
                       | Relation_op b
                       | Boolean_op c
   data ArithmeticOp = Plus | Minus | Mult | Div
   data RelationOp = Less | LessEq | Eq | Neq | GreaterEq | Greater
   data BooleanOp = And | Or
   data Alternative op numb v constr = Alternative numb [v] (Expression op numb v constr)
   data Alternatives op numb v constr  = Alternatives [(Alternative op numb v constr)] 
   data Definition op numb v constr  = Definition v (Expression op numb v constr)
   data Definitions op numb v constr = Definitions [(Definition op numb v constr)]
   data AtomicEx numb v constr op = AtomicN numb
                                  | AtomicV v
                                  | AtomicC (Constructor constr)
                                  | AtomicE  (Expression  op numb v constr )                        
   data Expression op numb v constr = Op op  (Expression  op numb v constr ) (Expression   op numb v constr ) 
                                    | App    (Expression  op numb v constr ) (Expression   op numb v constr )
                                    | Let    (Definitions op numb v constr ) (Expression   op numb v constr )
                                    | Letrec (Definitions op numb v constr ) (Expression   op numb v constr )
                                    | Case   (Expression  op numb v constr ) (Alternatives op numb v constr )
                                    | LambdaAbstraction [v] (Expression op numb v constr  )
                                    | Aex (AtomicEx numb v constr op )                                             
                                         
   data Constructor a  = Constructor a a
   data Program v ex = Program [(SuperComb v ex)]
   data SuperComb v ex = SuperComb [v] ex    

-- END DATA DEFS *********************************************************************************************************  
-- BEGIN PRINTER *********************************************************************************************************  
   


   printVar (Variable v) _= v
   printVars [] _= ""
   printVars (x:xs) n= (printVar x n) ++" "++ (printVars xs n)
   printNum (Number n) _= n
   printCon (Constructor a b) n= "Pack("++(printNum a n)++","++(printNum b n)++"# "
   printOp x _= case x of
                  Arithmetic_op y -> case y of
                     Plus     -> "+"
                     Minus    -> "-"
                     Mult     -> "*"
                     Div      -> "/"                     
                  Relation_op   y -> case y of
                     Less     -> "<"
                     LessEq   -> "<="
                     Eq       -> "=="
                     Neq      -> "~="
                     GreaterEq-> ">="
                     Greater  -> ">"
                  Boolean_op    y -> case y of
                     And      -> "&"
                     Or       -> "|"
   
   printAtomicExpr x n= case x of
                          AtomicV v -> printVar v n  
                          AtomicN u -> printNum u n
                          AtomicC c -> printCon c n
                          AtomicE (Aex ae) -> printAtomicExpr ae n
                          AtomicE e -> '(': (printExpr e n)++") "
                          
   printExpr x n= case x of
                    Op op a b -> ("("++(printExpr a n)++(printOp op n)++(printExpr b n)++")")   
                    App a b -> ((printExpr a n) ++" "++(printExpr b n)) 
                    Aex a -> printAtomicExpr a n                   
                    Let d e -> "let \n" ++(printSpace (n+1))++ (printDefs d (n+1))++" in "++(printExpr e n)
                    Letrec d e -> "letrec \n"++(printSpace (n+1))++(printDefs d (n+1))++" in "++(printExpr e n)
                    Case e a -> "case " ++ (printExpr e n)++" of\n"++(printSpace (n+1)) ++ (printAlts a (n+1))
                    LambdaAbstraction vs ex -> "\\"++ (printVars vs n)++" . "++(printExpr ex n)
    
   printAlt (Alternative n vs ex) u= "<"++(printNum n u)++"> "++(printVars vs u)++" -> " ++ (printExpr ex u)   
   printAlts (Alternatives [x]) n= printAlt x n
   printAlts (Alternatives (x:xs)) n= (printAlt x n) ++" ;\n" ++(printSpace n)++ (printAlts (Alternatives xs) n)
   
   printDef (Definition v ex) n= (printVar v n)++" = "++(printExpr ex n)
   
   printDefs (Definitions [x]) n= printDef x n  
   printDefs (Definitions (x:xs)) n= (printDef x n) ++ " ;\n"++(printSpace n)++(printDefs (Definitions xs) n)   
   
   printSC (SuperComb vs ex) n= (printVars vs n) ++ " = " ++(printExpr ex n)
   
   printProgram (Program [x]) n= printSC x n
   printProgram (Program (x:xs)) n= (printSC x n) ++" ;\n\n"++(printSpace n)++ (printProgram (Program xs) n)    
   
   printSpace 0 = ""
   printSpace n = "   "++( printSpace (n-1))
-- END PRINTER ***********************************************************************************************************
-- BEGIN PARSER **********************************************************************************************************


   program = do { skipMany whitespace 
                ; prg <- sepEndBy1 sc (do { many whitespace ; char ';' ; many whitespace})
                ; eof
                ; return (Program prg)
                }                

   sc = do { vs <- (sepEndBy var ( many space )) 
           ; char '='
           ; many whitespace
           ; e <- expr
           ; return (SuperComb vs e)
           }
  
   expr = buildExpressionParser table (do {many space; x<-nonrecexpr; many space;  return x})
           <|> nonrecexpr
           <?> "expression"

   table  = [[op "" (\x y->(App x y)) AssocLeft]
            ,[op "*" (\x y->(Op (Arithmetic_op(Mult)) x y)) AssocLeft, op "/" (\x y->(Op (Arithmetic_op(Div)) x y)) AssocNone]
            ,[op "+" (\x y->(Op (Arithmetic_op(Plus)) x y)) AssocLeft, op "-" (\x y->(Op (Arithmetic_op(Minus)) x y)) AssocNone]
            ,[op "==" (\x y->(Op (Relation_op Eq) x y)) AssocNone, op "~=" (\x y->(Op (Relation_op Neq) x y)) AssocNone, op ">" (\x y->(Op (Relation_op Greater) x y)) AssocNone, op ">=" (\x y->(Op (Relation_op GreaterEq) x y)) AssocNone, op "<" (\x y->(Op (Relation_op Less) x y)) AssocNone, op "<=" (\x y->(Op (Relation_op LessEq) x y)) AssocNone]
            ,[op "&" (\x y->(Op (Boolean_op And) x y)) AssocRight]
            ,[op "|" (\x y->(Op (Boolean_op Or) x y)) AssocRight]
            ]          
          where
          op s f assoc = Infix (do{string s;  return f}) assoc


   
   nonrecexpr = do { aex <- aexpr
                   ; return (Aex aex)
                   }
      <|> do { char '\\'
             ; vs <- (sepEndBy var (many space)) 
             ; char '.'
             ; many space
             ; ex <- expr
             ; return (LambdaAbstraction vs ex)
             }
      <|> do { string "case"
             ; many space
             ; e <- expr
             ; many space
             ; string "of"
             ; many1 space
             ; a <- alts
             ; return (Case e a)   
             }            
      <|> do { try (string "letrec") 
             ; many1 space
             ; d <- defns
             ; string "in"
             ; many1 space
             ; e <- expr
             ; return (Letrec d e)
             }
      <|> do { string "let" 
             ; many1 space
             ; d <- defns
             ; string "in"
             ; many space
             ; e <- expr
             ; return (Let d e)
             }             
             
   aexpr = try ( do { v <- var; 
                      return (AtomicV v)
                    })
       <|> do { n <-num
               ; return (AtomicN n) 
              }
       <|> do { string "Pack{"
              ; a <- num
              ; char ','
              ; b <- num
              ; char '}'
              ; return (AtomicC (Constructor a b))
              }
       <|> do { char '('
              ; many space
              ; ex <- expr
              ; many space
              ; char ')'
              ; return (AtomicE ex)
              }              
   
   defns = do { d1 <- def
              ; ds <- many (try (do { many whitespace 
                                    ; char ';'
                                    ; many whitespace
                                    ; d <- def
                                    ; return d
                                    }))
              ; return (Definitions (d1:ds))
              }              
                                    
  
   def = do { v <- var
            ; many space
            ; char '='
            ; many space
            ; ex <- expr
            ; return (Definition v ex)
            }            
   
   alts = do { a1 <- alt
             ; as <- many (try (do { many whitespace
                                   ; char ';'
                                   ; many whitespace
                                   ; a <- alt
                                   ; return a
                                   }))
             ; return (Alternatives (a1:as))
             }             
   
   alt = do { char '<'
            ; n <- num
            ; char '>'
            ; many space
            ; vars <- (sepEndBy var (many1 space))
            ; string "->"
            ; many whitespace
            ; ex <- expr          
            ; many space            
            ; return (Alternative n vars ex )
            }
  
   var = do { v <- letter
              ; vs <- many alphaNum
              ; case checkForKeyword (v:vs) keywords of 
                  Just x -> return (Variable x)
                  Nothing -> fail "keyword cannot be used as identificator"
                }
         <?> "identificator"    
         
   num = do { ns <- many1 (oneOf "0123456789")
            ; notFollowedBy letter
            ; return (Number ns)
            }
         <?> "number"
   
   
   whitespace = char ' '
            <|> char '\n'  
            <|> char '\t'
   
   keywords = ["Pack","case","of","letrec","let","in"]
   
   checkForKeyword x [] = Just x
   checkForKeyword y (x:xs) =  if x==y then Nothing
                                       else checkForKeyword y xs   
         
   whitespaces = many1 whitespace   
   
-- END PARSER ************************************************************************************************************   


   tst p input printer fhandle = case (parse p "" input) of
                     Left err -> return ("parse error at "++ (show err))
                     Right x  -> do { (IO.hPutStr fhandle (printer x 0))
                                    ; return "*------------------------*\n|   Parsing succesfull   |\n*------------------------*\n\n\n"
                                    }
   takename x [] = x
   takename x (y:ys) = if (y=='.') then x
                                   else (takename (x++[y]) ys)  
																	 
	 getProgram = do 
					filename <- IO.getLine
				  filehandle <- (IO.openFile filename IO.ReadMode)
				  contents <- (IO.hGetContents filehandle)
				  outhandle <- (IO.openFile ((takename [] filename)++".out") IO.WriteMode)
				  effect <- (tst program contents printProgram outhandle)
				  putStrLn effect
				  IO.hClose filehandle
				  IO.hClose outhandle
					
					
					
	 runProg :: [Char] -> [Char]
	 
	 compile :: Program -> TiState