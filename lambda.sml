use "set-sig.sml";
use "set.sml";

structure Lambda = 
struct 
   (* First import  the list-based set implementation to be used *)
   structure MSet :> Set = ListSet
   type 'a Set  = 'a MSet.Set
   (* Define a datatype for representing lambda terms as data-structures (trees!) *)
   datatype expr = 
	    var of string | 
	    apply of expr * expr |
	    abs of  expr * expr

   (* An exception to report an error when something terribly bad happens *)
   exception Bad of string
	
   (* toString is a convenience function to convert a lambda expression tree
      into a string that can be printed (useful for debugging!)
      *)	    
   fun toString (var x) = x
     | toString (apply(x,y))= "("^(toString x)^" "^ (toString y) ^ ")"
     | toString (abs(x,y))= "\\"^(toString x)^"."^(toString y)

   (* The printing routine simply uses toString to obtain a string and dunp it on the standard output.
      *)
   fun printIt x = print((toString x)^"\n")
    
    (* freeV computes the set of free variables appearing in the lambda term denoted by x
      *)
   fun freeV (abs(var x,y)) = MSet.remove x (freeV(y))
      |freeV (apply(x,y)) = MSet.union (freeV(x)) (freeV(y))
      |freeV (var x) = MSet.single x
      
   (* newName is a helper function charged with producing a name based on the "root" string (x) that does not
      belong to the set s. Namely, the output o is such that o \NOTIN s. For instance, if the root string is "foo"
      and the set s = { foo,foo_1,foo_2,foo_3}, a valid output would be foo_4
      *)    
   fun newName x s = 
	let fun newName1 i x s = if MSet.member (x^"_"^(Int.toString i)) s then
				    newName1 (i+1) x s
				 else
				    (x^"_"^(Int.toString i))
	in if MSet.member x s then
		newName1 0 x s
	else
		x
	end

   (* subst is meant to carry out a substitution. Namely, it computes [y/x]z, i.e., it replaces every free 
      occurences  of x by y within the lambda term z.
      *)
   fun subst x y (abs(var a,z)) = let
	val y0 = freeV y
	in if a=x then
		abs(var a,z)
	else if MSet.member a y0 then let
		val a0 = newName a y0
		val z0=subst a (var a0) z
	in abs(var a0,subst x y z0)	
	end
	else 
		abs(var a,subst x y z)
	end
	|subst x y (apply(z1,z2)) = apply(subst x y z1,subst x y z2)
	|subst x y (var a) = if a=x then
			y
		else
			var a 
       
   (* alpha implements the alpha-equivalence axiom and is responsible for producing a lambda term structurally equivalent
      to e but where any binder must use a variable name _not_ appearing in the set s. For instance, the lambda term
      Given a set s = {x}   the lambda term \x.\y.x y z    should be alpha renamed to \x_0.\y.x_0 y z to prevent any 
      accidental binding resulting from a beta reduction where \x.\y.x y is the body of the function 
      \z.\x.\y.x y z  and the argument is, for instance, x. Indeed, without alpha renaming, applying a direct substitution
      would incorrectly bind x! [x/z](\x.\y.x y z) ->beta  \x.\y.x y x  which is not the intended meaning. A suitable renaming
      of the binder does prevent the erroneous behavior.
      *)
   fun alpha (abs(var a,e)) s = if MSet.member a s then let 
	val a0=newName a s;
	in
		abs(var a0,subst a (var a0) e)
	end
	else
		abs(var a,alpha e s)
      |alpha (apply(e1,e2)) s = apply(alpha e1 s,alpha e2 s)
      |alpha (var a) s = if MSet.member a s then 
			var (newName a s) 
		else
			var a

   (* normal is meant to test whether the input lambda term is or is not in irreducible form. Recall that a lambda term
      is irreducible if and only if it does not contain any application of the form ((\x.M) B)
      *)
   fun normal e = let
	fun abnormal (apply(abs(x,M),B)) = true
      	   |abnormal (apply(a,b)) = if abnormal a then
		true
	    else 
		abnormal b 
  	   |abnormal (abs(a,z)) = abnormal z
      	   |abnormal (var a) = false 
	in not (abnormal e)
	end
			   
 
   (* beta implements the beta reduction discussed in class. Naturally, before reducing an application (A B) one should
      first make sure that no accidental binding can occur and, if they do, make use of the alpha reduction to carry out
      a suitable renaming.
      *)
   fun beta  e = let 
	fun beta1 (apply(abs(var x,z),y)) = subst x y z
	   |beta1 (apply(z1,z2)) = if not (normal z1) then
			apply(beta1 z1,z2)
		else 
			apply(z1,beta1 z2)
           |beta1 (abs(a,z)) = abs(a,beta1 z)
	   |beta1 (var a) = var a
	in if not (normal e) then
		beta1 e
	   else
		e
	end

  (* simp is the top-level lambda-calculus reduction function. It takes as input a lambda term e and recursively 
      reduces it until we obtain a normal form. The output is the irreducible answer.
      *)
   fun simp e = if not (normal e) then
		simp(beta e)
	else
		e
end
