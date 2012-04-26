module Utils where

	type Addr = Int
	type Name = String

-- Begin Heap Definitions --------------------------------------------------------------

	
	type Heap a = 	(Int			-- the number of objects in the heap
					,[Int]			-- a list of unused addresses
					, [(Addr, a)]) 	-- an association list mapping addresses to objects

	-- A number
	hInitial :: Heap a
	hAlloc   :: Heap a -> a -> (Heap a, Addr)
	hUpdate  :: Heap a -> Addr -> a -> Heap a
	hFree    :: Heap a -> Addr -> Heap a

	hInitial							= (0      , [1..]   , [])
	hAlloc	(size,(next:free), cts) n	= ((size+1, free	, (next,n):cts), next)
	hUpdate (size, free      , cts) a n = (size   , free	, (a,n):remove cts a)   
	hFree 	(size, free		 , cts) a 	= (size-1 , a:free	, remove cts a)

	hLookup :: (Show b) => (Int, [Int], [(Addr, b)]) -> Addr -> b
	hLookup (size, free, cts ) a
		= aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap \n" {-++ printHeap (size, free, cts )-} ))
		
	hAddresses (size, free, cts ) = [addr | (addr, node) <- cts]

	hSize (size, free, cts ) = size

	hNull = 0
	hIsnull a = a == 0
	showaddr a = "#" ++ show a

	remove :: [(Addr, a)] -> Addr -> [(Addr,a)]
	remove [] a = error ("Attempt to update or free nonexisten address " ++ showaddr a)
	remove ((a', n) : cts) a 	| a == a' = cts
								| a /= a' = (a', n) : remove cts a

	printHeap :: (Show b) => (Int, [Int], [(Addr, b)]) -> String	
	printHeap (0, _, _) = ""
	printHeap (size, [],  _ ) =  " Error - the size of a heap is not equal with the real size" 
	printHeap (size, d:ds, addr@(ad,val):addrs) = "Address [" ++ (fWNum 4 ad) ++ " || " ++ (show val) ++ "] \n"  ++ (printHeap (size-1, ds, addrs))
		
-- End Heap Definitions ----------------------------------------------------------------

-- Begin ASSOC Definitions -------------------------------------------------------------

	type ASSOC a b = [(a,b)]

	-- Search for a value connected with given key in a heap
	-- Shows the defined exception Message key has not been found
	aLookup :: (Eq a) => [(a, b)] -> a -> b -> b
	aLookup [] 					key' errM = errM
	aLookup ((key,v):bs) 	key' errM | key == key' = v
	aLookup ((key,v):bs) 	key' errM | key /= key' = aLookup bs key' errM
																
	aDomain :: ASSOC a b -> [a]
	aDomain alist = [key | (key, val) <- alist]

	aRange :: ASSOC a b -> [b]
	aRange alist = [val | (key, val) <- alist]

	aEmpty :: [a]
	aEmpty = []

-- End ASSOC Definitions ---------------------------------------------------------------

-- Begin Map Accumulator ---------------------------------------------------------------
	-- Useful combinator of map and foldl
	mapAccuml :: ( a -> b -> (a, c))  -- Function of accumulator and element
											-- 		input list, returnung new
											-- 		accumylator and element of result list
				-> a  						-- Initial accumulator
				-> [b]						-- Input list
				-> (a,[c])					-- Final accumulator and result list
							
	mapAccuml f acc [] 			= (acc, [])
	mapAccuml f acc (x:xs) 	
		= (acc2, x':xs')
			where
				(acc1, x') = f acc x
				(acc2, xs') = mapAccuml f acc1 xs

	-- Merge Sort
	sort [] = []
	sort [x] = [x]
	sort (x:xs) = [y | y <- xs, y < x] ++ x : [y | y <- xs, y >= x ]
	
	-- FWNum :: Int -> Int -> [Char]
	fWNum width n = space (width - length digits) ++ digits
		where	
			digits = show n
			
	space :: Int -> [Char]
	space 0 = []
	space n = ' ' : space (n-1)


														
-- End Map Accumulator -----------------------------------------------------------------