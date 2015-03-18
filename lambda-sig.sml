signature LExpr = 
sig
    exception Bad of string
    datatype expr = 
	     var of string | 
	     apply of expr * expr |
	     abs of  expr * expr
    type 'a Set
    val alpha : expr -> string Set -> expr
    val beta  : expr -> expr
    val simp  : expr -> expr
    val freeV : expr -> string Set
    val newName : string -> string Set -> string
    val printIt : expr -> unit
    val toString : expr -> string 
    val subst : string -> expr -> expr -> expr
end
