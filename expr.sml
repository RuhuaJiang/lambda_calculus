use "set-sig.sml";
use "set.sml";
use "lambda-sig.sml";
use "lambda.sml";
use "church-sig.sml";
use "church.sml";
         
structure Main = struct     
structure L : LExpr = Lambda;
structure C : ChurchSig = Church;
structure S : Set = ListSet;

(* We do not have a parser. To make things easy, I'm providing the definition of all the auxiliary 
   terms to define the recursive fibonacci function with lambda terms. The defitions include all the
   expected things (church numerals for the constants, lambda expressions for arithmetic operations, branching
   Y combinator, etc... Make sure you understand these definitions. Feel free to print them out (with your
   toString function to confirm you understand them.
 *)
fun fib (prog_name, args) = 
	let open L in
    let val t = abs(var "x",apply(var "f",apply(var "x",var "x")))    (* \x.f (x x)   *)
	    val Y = abs(var "f",apply(t,t))                               (* \f.\x.f (x x) \x.f (x x)  *)
	    val succ = abs(var "n",abs(var "s",abs(var "z", 
						   apply(var "s",
							 apply(apply(var "n",
								     var "s"),
							       var "z")))))
	    val zero = abs(var "s",abs(var "z",var "z"))                 (* Church numeral 0  *)
	    val tlam = abs(var "x",abs(var "y",var "x"))                 (* true =  \x.\y.x *)
	    val flam = abs(var "x",abs(var "y",var "y"))                 (* false = \x.\y.y *)
	    val isZ  = abs(var "n",apply(apply(var "n",abs(var "x",flam)),tlam))     (* isZero *)
	    val plus = abs(var "n",abs(var "m",apply(apply(var "n",succ),var "m")))  (* addition *)          	    
	    val mult = abs(var "n",abs(var "m",abs(var "f",apply(var "n",apply(var "m",var "f")))))   (* multiplication *)
	    val pair = abs(var "a",abs(var "b",abs(var "z",apply(apply(var "z",var "a"),var "b"))))   (* pairing *)
	    val fst  = abs(var "p",apply(var "p",tlam))
	    val snd  = abs(var "p",apply(var "p",flam))
	    val pp   = abs(var "p",apply(apply(pair,apply(snd,var "p")),apply(succ,apply(snd,var "p"))))
	    val pred = abs(var "n",apply(fst,apply(apply(var "n",pp),apply(apply(pair,zero),zero))))
	    val ifte = abs(var "c",abs(var "t",abs(var "e",apply(apply(var "c",var "t"),var "e"))))    (* if-then-else *)
	    val nm1  = apply(pred,var "n")                                                             (* n - 1 *)
	    val nm2 =  apply(pred,nm1)                                                                 (* n - 2 *)
	    val one = C.int2Church(1)
	    val four = C.int2Church(4)
	    val five = C.int2Church(5)
	    val seven = C.int2Church(7)
	    val fib  = abs(var "f",abs(var "n",
				       apply(apply(apply(ifte,
							 apply(isZ,var "n")),
						   zero),
					     apply(apply(apply(ifte,
							       apply(isZ,nm1)),
							 one),
						   apply(apply(plus,
							       apply(var "f",nm1)),
							 apply(var "f",nm2))))))
	    val fib4 = apply(apply(Y,fib),four)
	    val fib5 = apply(apply(Y,fib),five)
	    val fib7 = apply(apply(Y,fib),seven)
	    val fact = abs(var "f",abs(var "n",apply(apply(apply(ifte,apply(isZ,var "n")),one),apply(apply(mult,var "n"),apply(var "f",apply(pred,var "n"))))))
	    val fact1 = apply(apply(Y,fact),one)
	    val fact4 = apply(apply(Y,fact),four)
	    val test = apply(abs(var "z",abs(var "x",abs(var "x_0",apply(var "x",var "z")))),var "x")
	    val and1 = abs(var "a",abs(var "b",apply(apply(apply(ifte,var "a"),var "b"),flam))) 
	val sfact1=(simp fact1)
	val sfact4=(simp fact4)
	val sfib4=(simp fib4)
	val sfib5=(simp fib5)
	val sfib7=(simp fib7)
	val andTT=apply(apply(and1,tlam),tlam);
	val andTF=apply(apply(and1,tlam),flam);
	val andFT=apply(apply(and1,flam),tlam);
	val andFF=apply(apply(and1,flam),flam);

    in 
	print "Simplify fact(1):\n";
	printIt (sfact1);
	print ("Int: "^(Int.toString (C.church2Int sfact1))^"\n");
	print "Simplify fact(4):\n";
	printIt (sfact4);
	print ("Int: "^(Int.toString (C.church2Int sfact4))^"\n");
	print "Simplfy fib(4):\n";
	printIt (sfib4);
	print ("Int: "^(Int.toString (C.church2Int sfib4))^"\n");
	print "Simplfy fib(5):\n";
	printIt (sfib5);
	print ("Int: "^(Int.toString (C.church2Int sfib5))^"\n");
	print "Simplfy fib(7):\n";
	printIt(sfib7);
	print ("Int: "^(Int.toString (C.church2Int sfib7))^"\n");

	print "Input: ";
	printIt(test);
	print "Output: ";
	printIt(simp test);
	print "and(a,b): ";
	printIt(simp and1);
	print "and(T,T): ";
	printIt(simp andTT);
	print "and(T,F): ";
	printIt(simp andTF);
	print "and(F,T): ";
	printIt(simp andFT);
	print "and(F,F): ";
	printIt(simp andFF);
	1
    end
end
end;
