use "lambda-sig.sml";
use "lambda.sml";

structure Church : ChurchSig =
struct 
	exception Bad of string
	structure L : LExpr = Lambda
	type expr = L.expr;
	fun church2Int (L.abs(f,L.abs(x,e))) = let open L
		in let
		fun church2Int1 (var x) = 0
		   |church2Int1 (apply(f,x)) = (church2Int1 x)+1
		in church2Int1 e
		end
	end
	fun int2Church v = let open L
		in let 
		fun int2Church1 0 = var "x"
	   	   |int2Church1 v = apply(var "f",int2Church1 (v-1))
		in
		abs(var "f",abs(var "x",int2Church1 v))
		end
	end
end
