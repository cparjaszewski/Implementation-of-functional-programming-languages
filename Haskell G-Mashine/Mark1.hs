module Mark1 where
	import AST	
	import PrettyPrinter
	import Parser
	import Utils
	import qualified IO
	
-- Begin TI Prelude ----------------------------------------------------------------------
	type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
	type TiStack = [Addr]
	
	type TiDump = [TiStack] 
	initialTiDump :: TiDump
	initialTiDump = []
	
	type TiHeap = Heap Node
	
	data Node = NAp Addr Addr						-- Aplication
				| NSupercomb Name [Name] CoreExpr   -- Supercombinator
				| NNum Int 							-- Number
				| NInd Addr							-- Indirection
				| NPrim Name Primitive				-- Primitive
				| NData Int [Addr]					-- Tag, list of components
				
	data Primitive = Neg | Add | Sub | Mul | Div
	
	instance Show Node  where
		show node = "Show instance for Nodes has been not implemented yet"
		{-show (NAp a1 a2) 					= "NAp " ++ show a1 ++ " " ++ show a2
		show (NSupercomb name args expr)	= "NSupercomb " ++ show name ++ " [ " ++ show args ++ " ] " ++ (filter (\ch -> not (ch == '\n')) $ iDisplay (pprExpr expr))
		show (NNum n) 						= "NNum " ++ show n-}
-- End TI Prelude ------------------------------------------------------------------------

-- Begin Environment ---------------------------------------------------------------------							
	type TiGlobals = ASSOC Name Addr
	
	-- Add a definition to environment
	addDefToEnv :: (TiHeap, TiGlobals) -> CoreDef -> (TiHeap, TiGlobals)
	addDefToEnv (heap, env) (Df (name, expr)) 
		= (heap, (n, addr) : env)
			where (h, (n, addr)) = allocateSc heap (Sc (name, [], expr) )
-- End  Environment -----------------------------------------------------------------------
	
-- Begin Statistics -----------------------------------------------------------------------
	tiStatInitial :: TiStats
	tiStatIncSteps :: TiStats -> TiStats
	tiStatGetSteps :: TiStats -> Int
	
	type TiStats = Int
	tiStatInitial = 0
	tiStatIncSteps s = s+1
	tiStatGetSteps s = s
	
	-- Applies a given function to the statistics component of the state:
	applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
	applyToStats stats_fun (stack, dump, heap, sc_defs, stats)
		= (stack, dump, heap, sc_defs, stats_fun stats)
-- End Statistics -------------------------------------------------------------------------
	
-- Begin Compiler -------------------------------------------------------------------------
	compile :: CoreProgram -> TiState
	compile (Pr program)
		= (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
			where
				sc_defs = program ++ (fromPr preludeDefs) ++ extraPreludeDefs
				
				(initial_heap, globals) = buildInitialHeap sc_defs
				
				initial_stack = [address_of_main]
				address_of_main = aLookup globals "main" (error "main is not defined")
				
	-- A list of further standard function we may want to add; for present in is empty
	extraPreludeDefs = []

-- End Compiler --------------------------------------------------------------------------

-- Begin Heap ----------------------------------------------------------------------------

	buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
	buildInitialHeap sc_defs 
		= (heap2, sc_addrs ++ prim_addrs)
			where
				(heap1, sc_addrs) = mapAccuml allocateSc hInitial sc_defs
				(heap2, prim_addrs) = mapAccuml allocatePrim heap1 primitives
	
	allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
	allocateSc heap (Sc (name, args, body))
	 = (heap', (name, addr))
			where
				(heap', addr) = hAlloc heap (NSupercomb name args body)
				
	primitives :: ASSOC Name Primitive
	primitives = [ ("negate", Neg),
					("+", Add), ("-", Sub),
					("*", Mul), ("/", Div)
				]
	
	allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
	allocatePrim heap (name, prim)
		= (heap', (name, addr))
			where
				(heap', addr) = hAlloc heap (NPrim name prim)
-- End Heap ------------------------------------------------------------------------------

-- Begin Evaluator -----------------------------------------------------------------------
	eval state = state : rest_states
					where
					rest_states | tiFinal state = []
								| otherwise = eval next_state
					next_state = doAdmin (step state)
								
	--doAdmin will do any administrative work required between steps
	doAdmin :: TiState -> TiState
	doAdmin state = applyToStats tiStatIncSteps state
	
	tiFinal :: TiState -> Bool
	tiFinal ([sole_addr], dump, heap, globals, stats)
		=	isDataNode (hLookup heap sole_addr)
		
	tiFinal ([], dump, heap, globals, stats) = error "Empty Stack!"
	tiFinal state = False -- Stack contains more than one item
	
	isDataNode :: Node -> Bool
	isDataNode (NNum n) = True
	isDataNode node = False
	
	step :: TiState -> TiState
	step state 
		= dispatch (hLookup heap (head stack))
			where 
			(stack, dump, heap, globals, stats) = state
			
			dispatch (NNum n) 					= numStep state n
			dispatch (NAp a1 a2) 				= apStep state a1 a2
			dispatch (NSupercomb sc args body) 	= scStep state sc args body
			dispatch (NInd addr)				= indStep state addr
			dispatch (NPrim name prim) 			= primStep state prim
			
	numStep :: TiState -> Int -> TiState
	numStep state n = showError state "Number applied as a function!" 
	
	showError :: TiState -> [Char] -> a
	showError state msg = error (msg ++ (iDisplay $ showState state)) 
	
	
	apStep :: TiState -> Addr -> Addr -> TiState
	apStep (stack, dump, heap, globals, stats) a1 a2
		= (new_stack, dump, new_heap, globals, stats)
			where 
				node = hLookup heap a2
				(new_stack, new_heap) 
					= case node of
						(NInd a3) -> (stack, hUpdate heap (head stack) (NAp a1 a3))
						otherwise -> (a1 : stack, heap)

	-- After modification - using hUpdate to update the root of the redex with an indirection to the result
	scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
	scStep (stack, dump, heap, globals, stats) sc_name arg_names body
		= (new_stack, dump, new_heap, globals, stats)
			where
			upd_addr = head stack
			n = length arg_names
			new_stack = upd_addr : (drop (n + 1) stack)
			
			-- updated_new_heap = hUpdate new_heap a_n (NInd result_addr)
			new_heap = instantiateAndUpdate (body, upd_addr, heap, env)
			env = arg_bingings ++ globals
			arg_bingings = zip arg_names (getargs heap stack)

	-- Add an equation to the definition of dispatch to cope with indirections
	indStep :: TiState -> Addr -> TiState
	indStep (stack, dump, heap, globals, stats) addr
		= (new_stack, dump, new_heap, globals, stats)
			where
				old_header = head stack
				new_stack = addr : tail stack
				new_heap = hFree heap old_header
	
	primStep :: TiState -> Primitive -> TiState
	primStep state Neg 	= primNeg state
	primStep state Add 	= primArith state (+)
	primStep state Sub 	= primArith state (-)
	primStep state Mul 	= primArith state (*)
	primStep state Div 	= primArith state (div)
	
	primNeg :: TiState -> TiState
	primNeg (a:a1:[], dump, heap, globals, stats)
		= (new_stack, new_dump, new_heap, globals, stats)
			where 
				(NAp fun arg) = hLookup heap a1
				node = hLookup heap arg
				(new_stack, new_dump, new_heap) 
					= case node of	
						NNum n -> (a1:[], dump, hUpdate heap a1 (NNum (-n)))
						otherwise -> (arg:[], (a1:[]):dump, heap)
	
	primArith :: TiState -> (Int -> Int -> Int) -> TiState
	primArith state op = showError state "DEBUG\n"
	{-
		primArith (a:a1:a2:[], dump, heap, globals, stats) op
			= (new_stack, new_dump, new_heap, globals, stats)
				where
					(NAp fun1 arg1) = hLookup heap a1
					(NAp fun2 arg2) = hLookup heap a2
					node1 = hLookup heap arg1
					node2 = hLookup heap arg2
					(new_stack, new_dump, new_heap) 
						= case node1 of
							NNum n1 -> case node2 of
								NNum n2 -> (a1:[], dump, hUpdate heap a1 (NNum (n1 `op` n2)))
								otherwise -> (arg2:[], (a2:[]):dump, heap)
							otherwise -> (arg1:[], (a1:[]):dump, heap)
		primArith state _ = showError state "Error when doing arithmetic - some wrong values were on the stack\n"
	-}
	
	getargs :: TiHeap -> TiStack -> [Addr]
	getargs heap (sc:stack)
		= map get_arg stack
			where get_arg addr = arg 
				where (NAp fun arg) = hLookup heap addr
-- End Evaluator --------------------------------------------------------------------------

-- Begin Simple Instantiate	---------------------------------------------------------------		
	instantiateAndUpdate :: (CoreExpr 	-- Body of an supercombinator
		, Addr							-- Address of node to update
		, TiHeap						-- Heap before instantiation
		, ASSOC Name Addr				-- Associate parameters to addresses
		) -> TiHeap						-- Heap after instantiation

	instantiateAndUpdate (EAp e1 e2, upd_addr, heap, env) 
		= hUpdate heap2 upd_addr (NAp a1 a2)
			where
				(heap1, a1) = instantiate (e1, heap, env)
				(heap2, a2) = instantiate (e2, heap1, env)
	
	instantiateAndUpdate (EAEx (EVar v), upd_addr, heap, env)
		= hUpdate heap upd_addr found_node
			where
				found_node = hLookup heap (aLookup env v (error ("Undefined name " ++ show v)))
				
	instantiateAndUpdate (EAEx (ENum n), upd_addr, heap, env) 
		= hUpdate heap upd_addr (NNum n)
		
	-- Extremely hard - artithmetic
	instantiateAndUpdate (BiExpr (AritEx (op, e1, e2)), upd_addr, heap, env)
		= hUpdate heap4 upd_addr (NAp addrAp2 addrAp )
			where
				(heap1, a1) = instantiate (e1, heap, env)
				(heap2, a2) = instantiate (e2, heap1, env)
				(heap3, addrAp) = hAlloc heap2 (NAp addrPrim a1)
				(heap4, addrAp2) = hAlloc heap3 (NAp addrAp a2)
				addrPrim = aLookup env name (error ("Undefined prim " ++ show name))
				(name, prim) 
					= case op of
						Plus -> ("+", Add)
						Minus -> ("-", Sub)
						Mult -> ("*", Mul)
						Divide -> ("/", Div)
				
		
	instantiate :: (CoreExpr	-- Body of supercombinator
		, TiHeap				-- Association of names to addresses
		, TiGlobals		        -- Heap after instantiation
		) -> (TiHeap, Addr)		-- addres of root of instance				

	instantiate (EAEx (EVar v), heap, env)
		= (heap, aLookup env v (error ("Undefined name " ++ show v)))
									
	instantiate (EAEx (ENum n), heap, env) 
		= hAlloc heap (NNum n)
	
	instantiate ((EAp e1 e2), heap, env)
		= hAlloc heap2 (NAp a1 a2) where
				(heap1, a1) = instantiate (e1, heap, env)
				(heap2, a2) = instantiate (e2, heap1, env)
		
	instantiate ((EAEx (EConstr tag arity)), heap, env)
		= instantiateConstr tag arity heap env 
		
	instantiate ((EAEx (EInnerExpr expr)), heap, env)
		= instantiate (expr, heap, env)
	--  = error ("Can't instantiate inner expressions yet" ++ (iDisplay $ showStack heap []) )

	instantiate ((ELet defns body), heap, env)
		= instantiateLet defns body heap env
		
	instantiate ((ELetrec defns body), heap, env)
		= instantiateLetrec defns body heap env
		
	instantiateConstr tag arity heap env 
		= error "Can't instantiate constructors yet"
-- End Simple Instantiate	---------------------------------------------------------------	
		
-- Begin Non-recursive Let --------------------------------------------------------------			
		-- Let instantiotion Hints
		-- 1. instantiate the right hand side of each of the definitions in defns
		-- 2. augment the environment to bing the names in defns to the addresses of the newly constructed instances;
		-- 3. call instantiate passing the augmented environment and the expression body.
	
	instantiateLet :: [CoreDef] -> CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
	instantiateLet defns body heap env 
		= let	
				let_env = snd $ foldl addDefToEnv (heap, env) defns 
				let_heap = fst $ instantiate (body, heap, env)
			in instantiate (body, let_heap, let_env)

-- End Non-recursive Let -----------------------------------------------------------------	

-- Begin Recursive Let -------------------------------------------------------------------
		-- Letrec instantiotion Hints
		-- Copy your equation for the non-recursive ELet of instatiate and modify it to work for the recursive case (or modify
		--      your destination to deal with both
		-- Hint: do everything exactly as in the let case, except that in Step 1 pass the augmented environment
		--       (constructed in Step 2) to instantiate, instead of the existing environment
	
	instantiateLetrec :: [CoreDef] -> CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
	instantiateLetrec defns body heap env 
		= let
				letrec_env = snd $ foldl addDefToEnv (heap, env) defns 
				letrec_heap = fst $ instantiate (body, heap, letrec_env )
			in instantiate (body, letrec_heap, letrec_env)	

-- End Recursive Let ---------------------------------------------------------------------

-- Begin Formatter ------------------------------------------------------------------------
	showResults states
		= iDisplay (iConcat [ iLayn (map showState states)
								, showStats (last states)
								])
								
	showState :: TiState -> Iseq
	showState (stack, dump, heap , globals, stats)
		= iConcat [showStack heap stack, iNewline, iNewline, showHeap heap, iNewline]
		
	showHeapItem addr heap
		= iConcat [showFWAddr addr, iStr ": "
					, showHeapNode heap (hLookup heap addr)
					]
			
	showStack :: TiHeap -> TiStack -> Iseq
	showStack heap stack 
		= iConcat [
				iStr "STACK::  [", iNewline
				,iInterleave iNewline (map (\x -> showHeapItem x heap) stack)
				,iStr " ]"
			]
	
	showHeap :: TiHeap -> Iseq
	showHeap heap 
		= iConcat [
				iStr "Heap {{",iNewline 
				, iInterleave iNewline  (map showHeapItem2 heap_defs)
				,iStr " }}"
			]
			where
			heap_defs = let extract (_,_,x) = x in extract heap
			showHeapItem2 (addr, _) = showHeapItem addr heap
			
				
	showHeapNode :: TiHeap -> Node -> Iseq
	showHeapNode heap (NAp fun_addr arg_addr)
		= iConcat [ iStr "NAp "	, showFWAddr fun_addr
				, iStr " "		, showFWAddr arg_addr	
				, iStr " ("		, showNode (hLookup heap arg_addr)	
				, iStr " )"
			]
	showHeapNode heap node = showNode node
	
	showNode :: Node -> Iseq
	showNode (NAp a1 a2) = iConcat 	[ iStr " NAp " 	, showAddr a1
									, iStr " " 			, showAddr a2
									]
	showNode (NSupercomb name args body ) = iStr ("NSupercomb " ++ name )
	showNode (NNum n) = (iStr "NNum " ) `iAppend` (iNum n)
	showNode (NInd addr) = (iStr ("NInd # " ++ show addr))
	showNode (NPrim name prim) = (iStr ("NPrim (" ++ name ++ ")"))
	showNode (NData num args) = (iStr ("NData {" ++ show num ++ ", " ++ show args ++ "}"))
	
	
	showAddr :: Addr -> Iseq
	showAddr addr = iStr (show addr)
	
	-- Show address in field of width 4
	showFWAddr :: Addr -> Iseq
	showFWAddr addr = iStr (space (4 - length str) ++ str)
										where
										str = show addr

	showStats :: TiState -> Iseq
	showStats (stack, dump, heap, globals, stats)
		= iConcat [ iNewline, iNewline, iStr "Total number of steps = "
							, iNum (tiStatGetSteps stats)
			]

-- End Formatter --------------------------------------------------------------------------