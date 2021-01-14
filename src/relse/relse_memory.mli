open Relse_options

type index_t = Rel_expr.rel_bv
type value_t = Rel_expr.rel_bv
type t

val create : store_type:store_type -> t

val init_mem_at : addr:Bitvector.t -> size:int -> t -> unit

(** [address_belongs_to_init addr p] Check if the address [addr]
    belongs to the initialized memory locations of the symbolic state *)
val address_belongs_to_init: addr:Bitvector.t -> t -> bool

(** [store] If init is true, then the store is performed on a
   initialization phase and is not part of the analyzed
   program. [current_depth] is the current depth. [retire_depth] is
   the depth at which the store should be retired. [size] is the
   number of bytes to store.*)
val store : init:bool -> vaddr:Virtual_address.t -> current_depth:int -> retire_depth:int -> size:int ->
  index_t -> value_t -> Formula.formula -> t -> (Formula.formula * t)

(** [select] is a memory select. [current_depth] is the current depth. *)
val select : t -> vaddr:Virtual_address.t -> current_depth:int -> size:int -> index_t -> Relse_term.TransientSet.t

val add_declaration : t -> Formula.formula -> Formula.formula
