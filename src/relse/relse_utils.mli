type assignment_t =
  | Var of string * Size.Bit.t * Rel_expr.rel_bv (* name, value *)
  | Mem of Dba.size * Rel_expr.rel_bv * Rel_expr.rel_bv (* size, index, value *)

(* TODO: change to functions ? *)
val word_size_bits : unit -> Size.Bit.t        (* 32 bits *)
val word_size_bytes : unit -> Size.Byte.t      (* 32 bits *)
val bytesize_toint: int                        (* 8 *)

val is_sse : unit -> bool
val is_self_composed : unit -> bool
val is_relational : unit -> bool

val get_main_symbol: unit -> Loader.Symbol.t

val int_to_bitvector : int -> Bitvector.t

val dba_constant_from_int: int -> Dba.Expr.t

(** [is_loadable addr] Returns true if [addr] it is in a read-only
   section or a section specified in cmdline *)
val is_loadable :  Bitvector.t -> bool

(** Detect if the instruction is a conditional jump.
    Heuristic to detect conditional jump: has multiple outer targets. **)
val is_conditional_jump: Instruction.t -> bool

(** Detect if the instruction is a return statement **)
val is_return: Instruction.t -> bool

(** [read_bitvector bv size] Reads [size] bytes in the image of the
   executable at address [bv] *)
val read_bitvector : Bitvector.t -> int -> Bitvector.t

(** [temp_file ()] create a new temporary file *)
val temp_file : unit -> string

(** [mk_var_name basename idx] *)
val mk_var_name : string -> int -> string

type 'a formula_status =
  | Valid
  | Unsat
  | Sat of 'a
  | Unknown of 'a

val get_formula: Formula.bl_term formula_status -> Formula.bl_term

val solving_attempt : Formula.bl_term -> Formula.bl_term formula_status

val update_pc: Formula.bl_term Rel_expr.t ->
  Formula.bl_term formula_status -> Formula.bl_term formula_status

type comparison_type = Equal | Distinct | NotComparable
val compare_bv : Formula.bv_term -> Formula.bv_term -> comparison_type

(* val normalize_simple : Formula.bv_term -> Formula.bv_term -> Formula.bv_term
 * val normalize_rel : Formula.bv_term Rel_expr.t-> Formula.bv_term Rel_expr.t -> Formula.bv_term Rel_expr.t *)


(** Keep track of a list of addresses *)
module AddressSet : sig
  type t
  val size : t -> int
  val create : name:string -> t
  val add : Virtual_address.t -> t -> unit
  val pp_address_trace_to_file : t -> int -> unit

  (** [find addr al] Returns true if [addr] is in the address list [addr] *)
  val find: Virtual_address.t -> t -> bool
end

module AddressList : sig
  type t
  val create : name:string -> t
  val extend : Dba_types.Statement.t -> t -> t
  val pp_address_trace_to_file : t -> int -> unit

  (** [find addr al] Returns true if [addr] is in the address list [addr] *)
  val find: Dba_types.Statement.t -> t -> bool
end

module Spectre : sig  
  val unresolved_mem_access: 'a Rel_expr.t -> int -> bool
end


module F : sig

  val word_size: unit -> int ;;
  
  val var: string -> Formula.sort -> Formula.var ;;
  val def: Formula.term -> Formula.var -> Formula.def ;;
  val decl: Formula.var -> Formula.decl ;;

  val mk_initial_name: high:bool -> string -> string Rel_expr.t ;;
  val rel_name: high:bool -> string -> string Rel_expr.t ;;
  val with_index: string -> int -> string ;;

  val mk_bv: Formula.sort -> string -> Formula.bv_term ;;
  val mk_bl: string -> Formula.bl_term ;;

  val memory_name: string ;;
  val current_mem: high:bool -> int -> string Rel_expr.t ;;
  val memory_type: unit -> Formula.sort ;;
  val memory_term: string -> Formula.ax_term ;;
  val current_pc: int -> string ;;

  val normalize: string Rel_expr.t ->
    Formula.bv_term Rel_expr.t -> Formula.sort -> Formula.bv_term Rel_expr.t ;;

  val fml_add_entry: Formula.formula -> Formula.entry -> Formula.formula ;;
  val fml_assign: Formula.sort -> string -> Formula.term -> Formula.formula -> Formula.formula ;;
end
