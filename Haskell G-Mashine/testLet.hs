module TestLet where

	roots a b c =
		let det = sqrt (b*b - 4*a*c)
		in ((-b + det) / (2*a),
		(-b - det) / (2*a))