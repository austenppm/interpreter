type 'a t

val to_string : ('a -> string) -> 'a t -> string

val empty : 'a t

val extend : Syntax.id -> 'a -> 'a t -> 'a t

val lookup : Syntax.id -> 'a t -> 'a option

val map : ('a -> 'b) -> 'a t -> 'b t

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
