signature ChurchSig = 
sig
    type expr;
    val int2Church : int -> expr
    val church2Int : expr -> int 
end
