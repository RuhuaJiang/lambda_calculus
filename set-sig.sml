signature Set =
sig
    type ''a Set
    val single : ''a -> ''a Set
    val empty  : unit -> ''a Set
    val insert : ''a -> ''a Set -> ''a Set
    val union  : ''a Set -> ''a Set -> ''a Set
    val inter  : ''a Set -> ''a Set -> ''a Set
    val member : ''a -> ''a Set -> bool
    val remove : ''a -> ''a Set -> ''a Set
    val fromList: ''a list -> ''a Set			
    val show   : ''a Set -> (''a -> string) -> unit
end;
