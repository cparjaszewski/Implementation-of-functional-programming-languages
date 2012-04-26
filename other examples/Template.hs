module Template where
import Language
import Utils

compile::CoreProgram->TiState
eval::TiState->[TiState]

run::CoreProgram->[TiState]
run = eval.compile

-- Typy

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [UAddr]

data TiDump = DummyTiDump deriving (Show)
initialTiDump = DummyTiDump

type TiHeap = Heap Node
data Node
	= NAp UAddr UAddr 			-- Aplikacja
	| NSupercomb Name [Name] CoreExpr	-- Superkominator
	| NNum Int 				-- Liczba
	deriving (Show)

type TiGlobals = ASSOC Name UAddr

tiStatInitial :: TiStats
tiStatIncSteps :: TiStats -> TiStats
tiStatGetSteps :: TiStats -> Int

type TiStats = Int
tiStatInitial = 0
tiStatIncSteps s = s+1
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats)
	= (stack, dump, heap, sc_defs, stats_fun stats)

--
-- Kompilator
--
compile program
	= (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
	where
	sc_defs = program ++ preludeDefs ++ extraPreludeDefs

        (initial_heap, globals) = buildInitialHeap sc_defs

        initial_stack = [address_of_main]
	address_of_main = aLookup globals "main" (error "main nie jest zdefiniowana")

	extraPreludeDefs = []

buildInitialHeap::[CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = mapAccuml allocateSc hInitial sc_defs

allocateSc::TiHeap->CoreScDefn->(TiHeap, (Name, UAddr))
allocateSc heap (name, args, body)
	= (heap', (name, addr))
	where
	(heap', addr) = hAlloc heap (NSupercomb name args body)
--
-- Ewaluator
--
eval state = state : rest_states
	where
	rest_states
		| tiFinal state = []
		| otherwise = eval next_state
	next_state = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats)
	= isDataNode (hLookup heap sole_addr)

tiFinal ([], dump, heap, globals, stats) = error "Pusty stos!"
tiFinal state = False 	-- Stos zawiera wiecej, niz jeden obiekt

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node = False

step :: TiState -> TiState
step state
	= dispatch (hLookup heap (hd stack))
	where
	(stack, dump, heap, globals, stats) = state

	dispatch (NNum n) = numStep state n
	dispatch (NAp a1 a2) = apStep state a1 a2
	dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Aplikacja liczby!"

apStep :: TiState -> UAddr -> UAddr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2
	= (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
	= (new_stack, dump, new_heap, globals, stats)
	where
	new_stack = result_addr : (drop (length arg_names+1) stack)

	(new_heap, result_addr) = instantiate body heap env
	env = arg_bindings ++ globals
	arg_bindings = zip2 arg_names (getArgs heap stack)

getArgs :: TiHeap -> TiStack -> [UAddr]
getArgs heap (sc:stack)
	= map get_arg stack
	where get_arg addr = arg where (NAp fun arg) = hLookup heap addr

-- instantiate
instantiate :: CoreExpr 	-- Cialo superkombinatora
	-> TiHeap 		-- Sterta przed instantacja
	-> ASSOC Name UAddr 	-- Asocjacja nazw do adresów
	-> (TiHeap, UAddr) 	-- Sterta po instantacji
				-- oraz adres korzenia instancji
instantiate (ENum n) heap env = hAlloc heap (NNum n)

instantiate (EAp e1 e2) heap env
	= hAlloc heap2 (NAp a1 a2) where
		(heap1, a1) = instantiate e1 heap env
		(heap2, a2) = instantiate e2 heap1 env

instantiate (EVar v) heap env
	= (heap, aLookup env v (error ("Nie okreslona nazwa " ++ show v)))

--------------
simpleShow::[TiState]->String
simpleShow [] = "<<>>"
simpleShow (h:t) = (simpleShowState h) ++ " [] " ++ (simpleShow t)

simpleShowState::TiState->String
simpleShowState ([], _, _, _, _) = "<>"
simpleShowState (sh:st, d, h, f, stats) = (showHeap h sh) ++ " [] " ++ (simpleShowState (st, d, h, f, stats))

showHeap h s = show (hLookup h s)
