module TIM where
	import Parser
	import PrettyPrinter
	import Utils
	import AST


	-- The TIM addressing mode
	data TimAMode = Arg Int
		| Label [Char]
		| Code [Instruction]
		| IntConst Int
	type TimState = ([Instruction], -- The current instruction stream
		FramePtr, -- Address of current frame
		TimStack, -- Stack of arguments
		TimValueStack, -- Value stack (not used yet)
		TimDump, -- Dump (not used yet)
		TimHeap, -- Heap of frames
		CodeStore, -- Labelled blocks of code
		TimStats) -- Statistics
	data FramePtr = FrameAddr Addr -- The address of a frame
		| FrameInt Int -- An integer value
		| FrameNull -- Uninitialised
	instance Show FramePtr  where
		show fPtr = "FramPtr for TIM not implemented yet"
		
	
-- The stack contains closures, each of which is a pair containing a code pointer and a frame pointer. 
-- We represent the stack as a list.
	type TimStack = [Closure]
	type Closure = ([Instruction], FramePtr)
	type TimHeap = Heap Frame

	fAlloc :: TimHeap -> [Closure] -> (TimHeap, FramePtr)
	fGet :: TimHeap -> FramePtr -> Int -> Closure
	fUpdate :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
	fList :: Frame -> [Closure] -- Used when printing
	type Frame = [Closure]

	-- Allocates a xs value in the heap - returns a new heap and address to the xs value
	fAlloc heap xs = (heap', FrameAddr addr)
		where
			(heap', addr) = hAlloc heap xs

	-- Gets a value from a heap - by the given address
	fGet heap (FrameAddr addr) n = f !! (n-1)
		where
			f = hLookup heap addr

	fUpdate heap (FrameAddr addr) n closure
		= hUpdate heap addr new_frame
			where
				frame = hLookup heap addr
				new_frame = take (n-1) frame ++ [closure] ++ drop n frame

	fList f = f
	
	{- The code store component of the machine state now contains the address of the global frame and an association between supercombinator names and frame offsets-}
	type CodeStore =  ASSOC Name [Instruction]
	-- We take the opportunity to provide a lookup function for labels, which generates an error message if it fails
	codeLookup :: CodeStore -> Name -> [Instruction]
	codeLookup cstore l 
		= aLookup cstore l (error ("Attempt to jump to unknown label " ++ show l))
		
	-- As usual, we make the statistics into an abstract data type which we can add to easily:
	statInitial :: TimStats
	statIncSteps :: TimStats -> TimStats
	statGetSteps :: TimStats -> Int
	type TimStats = Int -- The number of steps
	statInitial = 0
	statIncSteps s = s+1
	statGetSteps s = s
	-- :a util.lhs -- heap data type and other library functions
	
	-- compile works very much like the template instantiation compiler, creating an initial machine state from the program it is given. 
	-- The main difference lies in the compilation function compileSC which is applied to each supercombinator.
	compile :: CoreProgram -> TimState
	compile program
		= ([Enter (Label "main")], -- Initial instructions
		FrameNull, -- Null frame pointer
		initialArgStack, -- Argument stack
		initialValueStack, -- Value stack
		initialDump, -- Dump
		hInitial, -- Empty heap
		compiled_code, -- Compiled code for supercombinators
		statInitial) -- Initial statistics
			where
				sc_defs = (fromPr preludeDefs) ++ (fromPr program)
				compiled_sc_defs = map (compileSC initial_env) sc_defs
				compiled_code = compiled_sc_defs ++ compiledPrimitives
				initial_env = [(name, Label name) | (name, args, body) <- (map fromSC sc_defs)] ++ [(name, Label name) | (name, code) <- compiledPrimitives]
				initialArgStack = []
				compiledPrimitives = []
				
	{- The heart of the compiler is a direct translation of the compilation schemes SC, R and A into the functions compileSC, compileR and compileA respectively. 
	The environment, \ro, is represented by an association list binding names to addressing modes. 
	The G-machine compiler used a mapping from names to stack offsets, but the extra flexibility of using addressing modes turns out to be rather useful. -}
	type TimCompilerEnv = [(Name, TimAMode)]
	compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])
	compileSC env (Sc (name, args, body))
		= (name, Take (length args) : instructions)
			where
				instructions = compileR body new_env
				new_env = (zip args (map Arg [1..])) ++ env
	
	compileR :: CoreExpr -> TimCompilerEnv -> [Instruction]
	compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
	compileR (EAEx (EVar v)) env = [Enter (compileA(EAEx (EVar v)) env)]
	compileR (EAEx (ENum n)) env = [Enter (compileA(EAEx (ENum n)) env)]
	compileR e env = error "compileR: can't do this yet"
	
	compileA:: CoreExpr -> TimCompilerEnv -> TimAMode
	compileA (EAEx (EVar v)) env = aLookup env v (error ("Unknown variable " ++ v))
	compileA (EAEx (ENum n)) env = IntConst n
	compileA e env = Code (compileR e env)
	
	eval state
		= state : rest_states 
			where
				rest_states | timFinal state = []
										| otherwise = eval next_state
				next_state = doAdmin (step state)

	doAdmin state = applyToStats statIncSteps state
	timFinal ([], frame, stack, vstack, dump, heap, cstore, stats) = True
	timFinal state = False
	
	applyToStats stats_fun (instr, frame, stack, vstack, dump, heap, cstore, stats)
			= (instr, frame, stack, vstack, dump, heap, cstore, stats_fun stats)

	-- Taking a step 
	-- step does the case analysis which takes a single instruction and executes it. The Take equation is a straightforward transliteration of the corresponding state transition rule	
	step :: TimState -> TimState
	step (((Take n):instr), fptr, stack, vstack, dump, heap, cstore, stats)
		| length stack >= n = (instr, fptr', drop n stack, vstack, dump, heap', cstore, stats)
		| otherwise = error "Too few args for Take instruction"
			where (heap', fptr') = fAlloc heap (take n stack)
	step ([Enter am], fptr, stack, vstack, dump, heap, cstore, stats) = (instr', fptr', stack, vstack, dump, heap, cstore, stats)	
		where (instr', fptr') = amToClosure am fptr heap cstore
	step (((Push am):instr), fptr, stack, vstack, dump, heap, cstore, stats) = (instr, fptr, (amToClosure am fptr heap cstore) : stack,	vstack, dump, heap, cstore, stats)
	step (((PushV f):instr), fptr@(FrameInt n), stack, vstack, dump, heap, cstore, stats) = 
		(instr, fptr, stack, n:vstack, dump, heap, cstore, stats)
	step (((PushV (IntVConst n)):instr),fptr, stack, vstack, dump, heap, cstore, stats) = (instr,  fptr, stack, n:vstack, dump, heap, cstore, stats)
	step ([Return], fptr, (i,f):stack, vstack, dump, heap, cstore, stats) = (i, f,stack, vstack, dump, heap, cstore, stats)
	step (((Op TAdd):instr), fptr, stack , n1:n2:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, (n1 + n2):vstack, dump, heap, cstore, stats)
	step (((Op TSub):instr), fptr, stack , n1:n2:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, (n1 - n2):vstack, dump, heap, cstore, stats)
	step (((Op TMult):instr), fptr, stack , n1:n2:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, (n1 * n2):vstack, dump, heap, cstore, stats)
	step (((Op TDiv):instr), fptr, stack , n1:n2:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, (n1 `div` n2):vstack, dump, heap, cstore, stats)
	step (((Op TNeg):instr), fptr, stack , n1:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, (-n1):vstack, dump, heap, cstore, stats)
	step (((Op TGr):instr), fptr, stack , n1:n2:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, v:vstack, dump, heap, cstore, stats)
		where v | (n1 > n2) 	= 1
						| otherwise 	= 0
	step (((Op TGrEq):instr), fptr, stack , n1:n2:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, v:vstack, dump, heap, cstore, stats)
		where v | (n1 >= n2) 	= 1
						| otherwise 	= 0
	step (((Op TLt):instr), fptr, stack , n1:n2:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, v:vstack, dump, heap, cstore, stats)
		where v | (n1 < n2) 	= 1
						| otherwise 	= 0
	step (((Op TLtEq):instr), fptr, stack , n1:n2:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, v:vstack, dump, heap, cstore, stats)
		where v | (n1 <= n2) 	= 1
						| otherwise 	= 0
	step (((Op TEq):instr), fptr, stack , n1:n2:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, v:vstack, dump, heap, cstore, stats)
		where v | (n1 == n2) 	= 1
						| otherwise 	= 0
	step (((Op TNotEq):instr), fptr, stack , n1:n2:vstack, dump, heap, cstore, stats) = (instr, fptr, stack, v:vstack, dump, heap, cstore, stats)
		where v | (n1 /= n2) 	= 1
						| otherwise 	= 0
	step([Cond i1 i2], fptr, stack, val:vstack, dump, heap, cstore, stats) = (i', fptr, stack, vstack, dump, heap, cstore, stats)
		where i' 
			| val == 0 	= i1
			| otherwise = i2
	step state@(_, fptr, stack, vstack, dump, heap, cstore, stats) = ([], fptr, stack, vstack, dump, heap, cstore, stats)
		

	-- amToClosure delivers the closure addressed by the addressing mode which is its first argument:
	amToClosure :: TimAMode -> FramePtr -> TimHeap -> CodeStore -> Closure
	amToClosure (Arg n) fptr heap cstore = fGet heap fptr n
	amToClosure (Code il) fptr heap cstore = (il, fptr)
	amToClosure (Label l) fptr heap cstore = (codeLookup cstore l, fptr)
	amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)
	
	{-As with the template instantiation version we need a rather boring collection of functions to print the results in a sensible way. It is often useful to print out the supercombinator definitions, so showResults begins by doing so, using the definitions in the first state-}
	showFullResults states
		= iDisplay (iConcat [
				iStr "Supercombinator definitions", iNewline, iNewline,
				showSCDefns first_state, iNewline, iNewline,
				iStr "State transitions", iNewline,
				iLayn (map showState states), iNewline, iNewline,
				showStats (last states)
				])
			where
				(first_state:rest_states) = states

	showResults states 
		= iDisplay (iConcat [
				showState last_state, iNewline, iNewline, showStats last_state
			])
			where last_state = last states

	showSCDefns :: TimState -> Iseq
	showSCDefns (instr, fptr, stack, vstack, dump, heap, cstore, stats)
		= iInterleave iNewline (map showSC cstore)
		
	showSC :: (Name, [Instruction]) -> Iseq
	showSC (name, il)
		= iConcat [
					iStr "Code for ", iStr name, iStr ":", iNewline,
					iStr " ", showInstructions Full il, iNewline, iNewline ]
	showState :: TimState -> Iseq
	showState (instr, fptr, stack, vstack, dump, heap, cstore, stats)
		= iConcat [
			iStr "Code: ", showInstructions Terse instr, iNewline,
			showFrame heap fptr,
			showStack stack,
			showValueStack vstack,
			showDump dump,
			iNewline
		]
		
	showFrame :: TimHeap -> FramePtr -> Iseq
	showFrame heap FrameNull = iStr "Null frame ptr" `iAppend` iNewline
	showFrame heap (FrameAddr addr)
		= iConcat [
					iStr "Frame: <",
					iIndent (iInterleave iNewline
					(map showClosure (fList (hLookup heap addr)))),
					iStr ">", iNewline
				]
	showFrame heap (FrameInt n)	= iConcat [ iStr "Frame ptr (int): ", iNum n, iNewline ]
	showStack :: TimStack -> Iseq
	showStack stack	= iConcat [ iStr "Arg stack: [",	iIndent (iInterleave iNewline (map showClosure stack)),	iStr "]", iNewline	]
	showValueStack :: TimValueStack -> Iseq
	showValueStack vstack = iNil
	showDump :: TimDump -> Iseq
	showDump dump = iNil
	showClosure :: Closure -> Iseq
	showClosure (i,f)
		= iConcat [ iStr "(", showInstructions Terse i, iStr ", ",
				showFramePtr f, iStr ")"
			]
	showFramePtr :: FramePtr -> Iseq
	showFramePtr FrameNull = iStr "null"
	showFramePtr (FrameAddr a) = iStr (show a)
	showFramePtr (FrameInt n) = iStr "int " `iAppend` iNum n
	showStats :: TimState -> Iseq
	showStats (instr, fptr, stack, vstack, dump, heap, code, stats)
		= iConcat [ iStr "Steps taken = ", iNum (statGetSteps stats), iNewline,
				iStr "No of frames allocated = ", iNum (hSize heap),
				iNewline
			]
	
	{-	We are going to need to print instructions and instruction sequences. If a sequence of instructions is printed as one long line, it is rather hard to read, so it is worth writing some code to prettyprint them.
In fact we want to be able to print either the entire code for an instruction sequence (for example when printing a supercombinator definition), or just some abbreviated form of it. An example of the latter occurs when printing the contents of the stack; it can be helpful to see some part of the code in each closure, but we do not want to see it all! Accordingly, we give an extra argument, d, to each function to tell it how fully to print. The value of this argument is either Full, Terse or None.	-}

	data HowMuchToPrint = Full | Terse | None
	showInstructions :: HowMuchToPrint -> [Instruction] -> Iseq
	showInstructions None il = iStr "{..}"
	showInstructions Terse il
		= iConcat [iStr "{", iIndent (iInterleave (iStr ", ") body), iStr "}"]
			where
				instrs = map (showInstruction None) il
				body 	| length il <= nTerse = instrs
						| otherwise = (take nTerse instrs) ++ [iStr ".."]
				
	showInstructions Full il
		= iConcat [iStr "{ ", iIndent (iInterleave sep instrs), iStr " }"]
			where
				sep = iStr "," `iAppend` iNewline
				instrs = map (showInstruction Full) il

	showInstruction :: HowMuchToPrint -> Instruction -> Iseq
	showInstruction d (Take m) = (iStr "Take ") `iAppend` (iNum m)
	showInstruction d (Enter x) = (iStr "Enter ") `iAppend` (showArg d x)
	showInstruction d (Push x) = (iStr "Push ") `iAppend` (showArg d x)
	
	showArg d (Arg m) = (iStr "Arg ") `iAppend` (iNum m)
	showArg d (Code il) = (iStr "Code ") `iAppend` (showInstructions d il)
	showArg d (Label s) = (iStr "Label ") `iAppend` (iStr s)
	showArg d (IntConst n) = (iStr "IntConst ") `iAppend` (iNum n)
	
	-- nTerse says how many instructions of a sequence should be printed in terse form.
	nTerse = 3
	
	intCode = [PushV FramePtr, Return]
	type TimValueStack = [Int]
	initialValueStack = []
	data Instruction = Take Int
		| Push TimAMode
		| PushMarker Int
		| PushV ValueAMode
		| Enter TimAMode
		| Return
		| Op Op
		| Cond [Instruction] [Instruction]
	
	instance Show Instruction where
		show x = "Show instruction not implemented"
		
	data Op = TAdd | TSub | TMult | TDiv 
		| TNeg 
		| TGr | TGrEq | TLt | TLtEq | TEq | TNotEq	deriving (Eq) 
	data ValueAMode = FramePtr
		| IntVConst Int
		
	initialArgStack = [([], FrameNull)]
	
	mkIndMode :: Int -> TimAMode
	mkIndMode n = Code [Enter (Arg n)]
	
	type TimDump 
		= [(FramePtr, -- The frame to be updated
				Int, -- Index of slot to be updated
				TimStack) -- Old stack
			]
	initialDump = []
	
	mkUpdIndMode :: Int -> TimAMode
	mkUpdIndMode n = Code [PushMarker n, Enter (Arg n)]
	
	mkEnter :: TimAMode -> [Instruction]
	mkEnter (Code i) = i
	mkEnter other_am = [Enter other_am]
	
{-	allocateInitialHeap :: [(Name, [Instruction])] -> (TimHeap, CodeStore)
	allocateInitialHeap compiled_code
		= (heap, (global_frame_addr, offsets))
			where
				indexed_code = zip [1..] compiled_code
				offsets = [(name, offset) | (offset, (name, code)) <- indexed_code]
				closures = [(PushMarker offset : code, global_frame_addr) | (offset, (name, code)) <- indexed_code]
				(heap, global_frame_addr) = fAlloc hInitial closures -}