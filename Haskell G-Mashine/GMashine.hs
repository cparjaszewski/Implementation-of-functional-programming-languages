module GMashine where
	import Parser
	import PrettyPrinter
	import Utils
	import AST

	
	-- The language of arithmetic expressions consists of: numbers, addition and multiplication. We can represent this language as the type aExpr.
	data AExpr = ANum Int
		| APlus AExpr AExpr
		| AMult AExpr AExpr
		| ALet [Def Name] AExpr

	-- It is intended that the language should have an ‘obvious' meaning; we can give this using the function aInterpret.
	aInterpret :: AExpr -> Int
	aInterpret (ANum n) = n
	aInterpret (APlus e1 e2) = aInterpret e1 + aInterpret e2
	aInterpret (AMult e1 e2) = aInterpret e1 * aInterpret e2
	-- aInterpret (ALet: is, ?????????????????????????
	
	-- We can give the instructions for our postfix machine as the type aInstruction.
	data AInstruction = INum Int
		| IPlus
		| IMult
		| ILet
		
	
	-- The state of the evaluator is a pair, which is a sequence of operators and a stack of numbers.
	-- The meaning of a code sequence is then given in the following transition rules.	
	aEval :: ([AInstruction], [Int]) -> Int
	aEval ([], [n]) = n
	aEval (INum n:is, s) = aEval (is, n: s)
	aEval (IPlus: is, n0:n1:s) = aEval (is, n1+n0:s)
	aEval (IMult: is, n0:n1:s) = aEval (is, n1*n0:s)
	-- aEval (ILet: is, ?????????????????????????
	
	-- To generate the sequence of postfix code for an expression we must define a compiler. 
	-- This takes an expression and delivers a sequence of instructions, which when executed will compute the value of the expression.
	aCompile :: AExpr -> [AInstruction]
	aCompile (ANum n) = [INum n]
	aCompile (APlus e1 e2) = aCompile e1 ++ aCompile e2 ++ [IPlus]
	aCompile (AMult e1 e2) = aCompile e1 ++ aCompile e2 ++ [IMult]
	-- aCompile (ALet ? ?? ) = ??????????????????????????????
	
	
	
	-- The function run is already defined in gofers standard.prelude
--	runProg :: [Char] -> [Char]
--	runProg = showResults . gmEval . gmCompile . parse
	
-- Begin Types ----------------------------------------------------------------------

	-- The Mark 1 G-machine uses the five-tuple, gmState, as its state. 
	-- A gmState holds all the information that we need during the execution of the compiled program.
	type GmState
		= (GmCode, -- Current instruction stream
		GmStack, -- Current stack
		GmHeap, -- Heap of nodes
		GmGlobals, -- Global addresses in heap
		GmStats) -- Statistics
	
	-- The instruction stream is of type gmCode and is simply a list of instructions.
	type GmCode = [Instruction]
	
	-- To get convenient access to the code, when the state is later augmented with extra components,
	-- we define two functions: getCode and putCode.
	getCode :: GmState -> GmCode
	getCode (i, stack, heap, globals, stats) = i

	putCode :: GmCode -> GmState -> GmState
	putCode i' (i, stack, heap, globals, stats) = (i', stack, heap, globals, stats)
	
	-- The G-machine stack gmStack is a list of addresses in the heap.
	type GmStack = [Addr]
	
	-- To get convenient access to the stack, when the state is later augmented with extra components,
	-- we define two functions getStack and putStack
	getStack :: GmState -> GmStack
	getStack (i, stack, heap, globals, stats) = stack
	
	putStack :: GmStack -> GmState -> GmState
	putStack stack' (i, stack, heap, globals, stats) = (i, stack', heap, globals, stats)
	
	-- Just as we did in the case of the template instantiator, we use the heap data structure from utils to implement heaps.	
	type GmHeap = Heap Node
	
	-- Again, to access this component of the state we define access functions.
	getHeap :: GmState -> GmHeap
	getHeap (i, stack, heap, globals, stats) = heap

	putHeap :: GmHeap -> GmState -> GmState	
	putHeap heap' (i, stack, heap, globals, stats) = (i, stack, heap', globals, stats)
	
	-- Because we will later be making a lazy implementation it is important that there is only one node for each global. The address of a global can be determined by looking up its value in the association list gmGlobals. This corresponds to the tiGlobals component of the template machine.
	type GmGlobals = ASSOC Name Addr
	
	-- The access function we use is getGlobals; in the Mark 1 machine, this component is constant so we do not need a corresponding put function.
	getGlobals :: GmState -> GmGlobals
	getGlobals (i, stack, heap, globals, stats) = globals
	
	-- The statistics component of the state is implemented as an abstract data type.
	statInitial :: GmStats
	statIncSteps :: GmStats -> GmStats
	statGetSteps :: GmStats -> Int
	
	-- The implementation of gmStats is now given.
	type GmStats = Int
	statInitial = 0
	statIncSteps s = s+1
	statGetSteps s = s

	-- To access this component we define getStats and putStats:
	getStats :: GmState -> GmStats
	getStats (i, stack, heap, globals, stats) = stats

	putStats :: GmStats -> GmState -> GmState
	putStats stats' (i, stack, heap, globals, stats) = (i, stack, heap, globals, stats')

	
-- End   Types ----------------------------------------------------------------------

-- Begin Instructions ---------------------------------------------------------------
	-- There are only six instructions initially.
	data Instruction
		= Unwind
		| Pushglobal Name
		| Pushint Int
		| Push Int
		| Mkapp
		| Slide Int
		
	instance Eq Instruction	where
		Unwind == Unwind = True
		Pushglobal a == Pushglobal b = a == b
		Pushint a == Pushint b = a == b
		Push a == Push b = a == b
		Mkapp == Mkapp = True
		Slide a == Slide b = a == b
		_ == _ = False
		
-- End   Instructions ---------------------------------------------------------------

-- Begin Nodes ----------------------------------------------------------------------
	-- In the minimal G-machine there are only three types of nodes: 
	-- numbers, NNum; 
	-- applications, NAp; 
	-- and globals, NGlobal.
	data Node
		= NNum Int -- Numbers (Number nodes contain the relevant number)
		| NAp Addr Addr -- Applications (Application nodes apply the function at the first address to the expression at the second address)
		| NGlobal Int GmCode -- Globals (The NGlobal node contains the number of arguments that the global expects and the code sequence to be executed when the global has enough arguments. This replaces the NSupercomb nodes of the template instantiator, which held a template instead of the arity and code.)
		
-- End   Nodes ----------------------------------------------------------------------

-- Begin Evaluator ------------------------------------------------------------------
	-- The G-machine evaluator, gmEval, is defined to produce a list of states. 
	-- The first one is the one constructed by the compiler. If there is a last state, then the result of the evaluation will be on the top of the stack component of the last state.
	gmEval :: GmState -> [GmState]
	gmEval state = state: restStates
		where
			restStates 	| gmFinal state = []
						| otherwise 	= gmEval nextState
			nextState = gmDoAdmin (gmStep state)
			
	-- The function gmDoAdmin uses statIncSteps to modify the statistics component of the state.
	gmDoAdmin :: GmState -> GmState
	gmDoAdmin s = putStats (statIncSteps (getStats s)) s
	
	-- The important parts of the evaluator are the functions gmFinal and gmStep which we will now look at. 
	
	-- "TESTING FOR A FINAL STATE" : The G-machine interpreter has finished when the code sequence that it is executing is empty.
	-- We express this condition in the gmFinal function.
	gmFinal :: GmState -> Bool
	gmFinal s 
		= case (getCode s) of
		[] -> True
		otherwise -> False
		
	-- "TAKING A STEP" : The gmStep function is defined so that it makes a state transition based on the instruction it is executing.
	gmStep :: GmState -> GmState
	gmStep state = dispatch i (putCode is state)
		where (i:is) = getCode state
		
	-- We dispatch on the current instruction i and replace the current code sequence with the code sequence is;
	-- this corresponds to advancing the program counter in a real machine.
	-- As we can see, the dispatch function simply selects a state transition to execute.
	dispatch :: Instruction -> GmState -> GmState
	dispatch (Pushglobal f) = pushglobal f
	dispatch (Pushint n) = pushint n
	dispatch Mkapp = mkapp
	dispatch (Push n) = push n
	dispatch (Slide n) = slide n
	dispatch Unwind = unwind
	
	-- The Pushglobal instruction uses the globals component of the state to find the unique NGlobal node in the heap that holds the global f . 
	-- If it cannot find one, it prints a suitable error message.
	pushglobal :: Name -> GmState -> GmState
	pushglobal f state
		= putStack (a: getStack state) state
			where 
				a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))
				
	-- The corresponding function is pushint. The number is placed in the new heap heap' with address a. 
	-- We then place the heap and stack back into the state.
	pushint :: Int -> GmState -> GmState
	pushint n state
		= putHeap heap' (putStack (a: getStack state) state)
			where 
				(heap', a) = hAlloc (getHeap state) (NNum n)
			
	-- The Mkapp instruction uses the two addresses on the top of the stack to construct an application node in the heap. 
	-- It has the following transition rule.
	mkapp :: GmState -> GmState
	mkapp state
		= putHeap heap' (putStack (a:as') state)
			where 
				(heap', a)  = hAlloc (getHeap state) (NAp a1 a2)
				(a1:a2:as') = getStack state
				
	-- The Push instruction is used to take a copy of an argument which was passed to a function. 
	-- To do this it has to ‘look through’ the application node which is pointed to from the stack. 
	-- We must also remember to skip over the supercombinator node which is on the stack.
	push :: Int -> GmState -> GmState
	push n state
		= putStack (a:as) state
			where 
				as = getStack state
				a  = getArg (hLookup (getHeap state) (as !! (n+1)))
				
	-- This uses the auxiliary function getArg to select the required expression from an application node.
	getArg :: Node -> Addr
	getArg (NAp a1 a2) = a2
	
	-- Next, the tidying up of the stack, which occurs after a supercombinator has been instantiated and before continuing unwinding, is performed by the Slide instruction.
	slide :: Int -> GmState -> GmState
	slide n state	
		= putStack (a: drop n as) state
			where 
				(a:as) = getStack state
				
	-- Unwind is the most complex instruction because it replaces the outer loop of our template instantiator. 
	-- The Unwind instruction is always the last instruction of a sequence, as we shall see in the next section. 
	-- The newState constructed depends on the item on top of the stack; this depends on the transition rule that is fired, 
	-- which also depends on the item on top of the stack.
	unwind :: GmState -> GmState
	unwind state
		= newState (hLookup heap a)
			where
				(a:as) = getStack state
				heap   = getHeap state
				-- We first consider the case where there is a number on top of the stack. 
				-- In this case, we are finished; the G-machine has terminated, and we place [] in the code component to signify this fact.
				newState (NNum n) = state
				-- If there is an application node on top of the stack then we must continue to unwind from the next node.
				newState (NAp a1 a2) = putCode [Unwind] (putStack (a1:a:as) state)
				-- Firstly, if there are not enough arguments to reduce the supercombinator application then the program was ill-typed. 
				-- We will ignore this case for the Mark 1 G-machine. 
				-- Alternatively, when there are enough arguments, it is possible to reduce the supercombinator, by ‘jumping to’ the code for the supercombinator.
				newState (NGlobal n c)
					| length as < n = error "Unwinding with too few arguments"
					| otherwise = putCode c state
				
-- End   Evaluator ------------------------------------------------------------------

-- Begin Compiler -------------------------------------------------------------------
	-- The gmCompile function turns a program into an initial state for the G-machine. 
	-- The initial code sequence finds the global main and then evaluates it. 
	-- The heap is initialised so that it contains a node for each global declared. 
	-- globals contains the map from global names to the NGlobal nodes provided for them.
	gmCompile :: CoreProgram -> GmState
	gmCompile program
		= (initialCode, [], heap, globals, statInitial)
			where 
				(heap, globals) = gmBuildInitialHeap program
				
	gmBuildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
	gmBuildInitialHeap program
		= mapAccuml gmAllocateSc hInitial compiled
			where compiled = map gmCompileSc program
			
	-- The buildInitialHeap function uses mapAccuml to allocate nodes for each compiled global; 
	-- the compilation occurring (where necessary) in compiled, which has type [gmCompiledSC].
	type GmCompiledSC = (Name, Int, GmCode)
	
	-- The function gmAllocateSc allocates a new global for its compiled supercombinator argument, returning the new heap and the address where the global is stored.
	gmAllocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
	gmAllocateSc heap (name, nargs, instns)
		= (heap', (name, addr))
			where 
				(heap', addr) = hAlloc heap (NGlobal nargs instns)
				
	-- In the initial state, we want the machine to evaluate the value of the program. We recall that this is just the value of the global main.
	initialCode :: GmCode
	initialCode = [Pushglobal "main", Unwind]
	
	-- Supercombinator is compiled using gmCompileSc. 
	-- It returns a triple containing the supercombinator name, the number of arguments the supercombinator needs before it can be reduced, and the code sequence associated with the supercombinator.
	gmCompileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
	gmCompileSc (name, env, body)
		= (name, length env, gmCompileR body (zip2 env [0..]))
		
	-- This in turn uses gmCompileR, which corresponds to the R scheme of Figure 3.3.
	gmCompileR :: GmCompiler
	gmCompileR e env = gmCompileC e env ++ [Slide (length env + 1), Unwind]
	
	-- Each of the compiler schemes has the same type: gmCompiler.
	type GmCompiler = CoreExpr -> GmEnvironment -> GmCode
	
	-- We use the fact that we can represent the map ñ from the compilation scheme as an association list. 
	-- Not only can we look up the offsets for a variable from this list, but we may also calculate how many arguments there are on the stack. 
	-- This is used in compileR to find out how many stack elements to squeeze out with a Slide instruction. 
	-- The list has type gmEnvironment, which is defined as:
	type GmEnvironment = ASSOC Name Int
	
	-- This constructs the instantiation of the supercombinator body using compileC, which corresponds
	-- to the C scheme of Figure 3.3.
	gmCompileC :: GmCompiler
	gmCompileC (EVar v) env
		| elem v (aDomain env) = [Push n]
		| otherwise = [Pushglobal v]
			where n = aLookup env v (error "Can't happen")
	gmCompileC (ENum n) env = [Pushint n]
	gmCompileC (EAp e1 e2) env 
		= gmCompileC e2 env ++ gmCompileC e1 (argOffset 1 env) ++ [Mkap]

	-- We can change the stack offsets using the function argOffset. If env implements ñ, then
	--(argOffset n env) implements R^(+n).
	argOffset :: Int -> GmEnvironment -> GmEnvironment
	argOffset n env = [(v, n+m) | (v,m) <- env]
	
-- End   Compiler -------------------------------------------------------------------