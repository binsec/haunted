type value_t = Rel_expr.rel_bv
type index_t = Rel_expr.rel_bv
type constraint_t = Rel_expr.rel_pc

module State : sig
  type t

  (** [address_belongs_to_init addr p] Check if the address [addr]
      belongs to the initialized memory locations of the symbolic state *)
  val address_belongs_to_init: addr:Bitvector.t -> t -> bool

  (** [init_mem_at addr size st] Initializes the memory of the symbolic state
      [st] at address [addr] with size [size] *)
  val init_mem_at : addr:Bitvector.t -> size:int -> t -> t
  
  (** [create store] Create a new state with store [store] *)
  val create : unit -> t

  (** [add_assert st h]  Add the assertion [h] to the state [st]*)
  val add_assertion : Formula.bl_term -> t -> t
  
  val declare_high : string -> Formula.sort -> t -> t
  val declare_low  : string -> Formula.sort -> t -> t
  
  (** [comment cmt st] Add the comment [cmt] to the current formula in [st] *)
  val comment : string -> t -> t

  (** [untaint r_expr st] If the untainting option is set and [r_expr
     = <r_expr_l|r_expr_r>] is relational, deduces the variables
     [<v_l|v_r>] that must be equal in both sides of the expression
     given that [r_expr_l] and [r_expr_r] are equal, and add them to
     variables to untaint (replace further [v_r] occurences with [v_l]
     occurences). *)
  (* val untaint_bl : Rel_expr.rel_pc -> t -> (Rel_expr.rel_pc * t) *)
  val untaint_bv : Rel_expr.rel_bv -> t -> t
  

  val pc_update : ?checked:bool -> t -> constraint_t -> t
  val var_assign : t -> string -> Size.Bit.t -> value_t -> t
  val memory_store : init:bool -> vaddr:Virtual_address.t -> current_depth:int -> retire_depth:int ->
    t -> int -> index_t -> value_t -> t

  (** Return the current path constraint *)
  val path_constraint : current_depth:int -> t -> Formula.bl_term Relse_utils.formula_status

  (* (\** Mark current path constraint as satisfiable  *\)
   * val mark_sat: t -> t *)
  
  (** [var_load st name size] Returns the value of the variable [name]
     in the symbolic store *)
  val var_load : current_depth:int -> t -> string -> Size.Bit.t -> value_t

  (** [memory_select st size r_index] Selects [size] bits in memory at
     index [r_index] *)
  val memory_select : vaddr:Virtual_address.t -> current_depth:int -> t ->
    Dba.size -> index_t-> Relse_term.TransientSet.t

  (** Returns the current formula of the symbolic state *)
  val formula : retire_stl:bool -> current_depth:int -> t -> Formula.formula

  val mark_sat : t -> t
  val mark_unsat :  t -> t
  val mark_unknown :  t -> t  

  val fence : t -> t
end
