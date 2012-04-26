type MultState = (Int, Int, Int, Int)

evalMult::MultState->[MultState]
evalMult state =
	if multFinal state
		then [state]
		else state : evalMult (stepMult state)

stepMult::MultState->MultState
stepMult (n, m, d, t) | d >  0 = (n, m,   d-1, t+1)
stepMult (n, m, d, t) | d == 0 = (n, m-1, n,   t)

multFinal::MultState->Bool
multFinal (_, 0, 0, _) = True
multFinal _ = False

type FactState = (Int, Int)

evalFact::FactState->[FactState]
evalFact state =
	if factFinal state
		then [state]
		else state : evalFact (stepFact state)

stepFact::FactState->FactState
stepFact (n, t) | n > 1 = (n-1, t*n)

factFinal::FactState->Bool
factFinal (1, _) = True
factFinal _ = False
