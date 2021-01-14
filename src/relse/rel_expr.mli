(** Module representing relational expressions
    [Rel e e'] is the type for relational expression
    [Simple e] is the type for non-relational expressions
*)
type proj_t = Left | Right | Value
type 'a relational = Rel of 'a * 'a | Simple of 'a
exception Should_not_be_relational of string

type 'a t

type rel_memory = Formula.ax_term t
type rel_pc = Formula.bl_term t
type rel_bv = Formula.bv_term t

(** [is_relational expr] Returns [true] if [expr] is a relational expression and
    [false] if it is a simple expression *)
val is_relational : 'a t -> bool
val get_value : 'a t -> 'a relational

val mk_rel : ?date:int option -> 'a -> 'a -> 'a t
val mk_simple : ?date:int option -> 'a -> 'a t

(** [deduplicate r_expr] turns the relational expression [Rel e e']
    into a simple expression if [e = e']*)
val deduplicate : 'a t -> 'a t
val deduplicate_eq : ('a -> 'a -> bool) -> 'a t -> 'a t

(** [left r_expr]
    Returns the left value of the relational expression [r_expr]
    or its unique value if it is [Simple] *)
val left : 'a t -> 'a
val r_left: 'a t -> 'a t

(** [right r_expr]
    Returns the right value of the relational expression [r_expr]
    or its unique value if it is [Simple] *)
val right : 'a t -> 'a
val r_right: 'a t -> 'a t

(** [value r_expr]
    Returns the value of the simple expression [r_expr]
    Raises [Invalid_argument] if the expression is not [Simple] *)
val value : 'a t -> 'a

(** [proj p r_expr] Returns the value of the relational expression
    [r_expr] corresponding to the projection [p], or its unique value
    if it is [Simple] *)
val proj : proj_t -> 'a t -> 'a

(** [apply f r_expr] Applies the function [f] to the relational expression [r_expr] *)
val apply : ('a -> 'b) -> 'a t -> 'b t

(** [apply2 f r_expr1 r_expr2] Applies the relational function [f] to
    the relational expressions [r_expr1] and [r_expr2] *)
val apply2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** [apply3 f r_expr1 r_expr2 r_expr3] Applies the relational function [f] to
    the relational expressions [r_expr1], [r_expr2] ans [r_expr3]*)
val apply3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

val apply_lr : ('a -> 'a -> 'b) -> 'a t -> 'b
val apply_left: ('a -> 'a) -> 'a t -> 'a t
val apply_right: ('a -> 'a) -> 'a t -> 'a t

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c

(** [to_string f r_expr] Pretty print the relation expression [r_expr] according to function [f] *)
val to_string : ('a -> string) -> 'a t -> string
val pp_rexpr: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(** [equals f r_e r_e'] True if r_e and r_e' are equal as defined by
   the function [f] (and their dates are equal) *)
val equals: ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

(** [equals f r_e r_e'] True if r_e and r_e' are equal as defined by
   the function [f] (even if dates are not equal) *)
val val_equals: ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

(** [hash f r_e] Computes the [hash] of [r_e] as defined by the function [f] *)
val hash: ('a -> 'b) -> 'a t -> int

(** { For dated Rexpr } *)

val get_date : 'a t -> int option
val set_date : int option -> 'a t -> 'a t  
