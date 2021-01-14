type 'a status_t = Continue of 'a | Halt

module Spectre :
sig
  type mode = TransientMode
            | RegularMode (* Non transient execution *)
            | HauntedMode
end

(** The symbolic state and symbolic path of the current symbolic execution *)
module Path_state :
sig
  type t

  (** Current depth of the symbolic execution *)
  val depth : t -> int

  (** Create a new path *)
  val create :
    ?depth:int -> ?path:Relse_utils.AddressList.t option -> ?block_index:int ->
    Relse_symbolic.State.t -> Instruction.t -> t

  (** If no-comment option is disabled, add a comment to the symbolic
      state *)
  val maybe_add_comment : t -> string -> t

  (** Returns the current instuction *)
  val get_instruction : t -> Instruction.t

  (** Returns the current instuction in a DBA form *)
  val get_dba_instruction : t -> Dba.Instr.t


  (** { Accessors to symbolic state }  *)

  val formula: retire_stl:bool -> t -> Formula.formula
  val path_constraint: t -> Formula.bl_term Relse_utils.formula_status
  val var_load: t -> string -> Size.Bit.t -> Relse_symbolic.value_t
  val memory_select: t -> Dba.size -> Relse_symbolic.index_t -> Relse_term.TransientSet.t
  val untaint: t -> Rel_expr.rel_bv list -> t
  val add_assertion: t -> Formula.bl_term -> t
  val declare_low: string -> Formula.sort -> t -> t
  val declare_high: string -> Formula.sort -> t -> t

  (** Get the virtual address of the current instruction *)
  val virtual_address : t -> Virtual_address.t

  (** Get the address of the current instruction as a Caddress *)
  val location : t -> Dba_types.Caddress.t

  (** Get the current statement (Caddr + DBA instuction) *)
  val get_current_statement : t ->  Dba_types.Statement.t

  (** [get_block_index ps] Returns the block index of [ps] *)
  val get_block_index : t -> int

  (* val leads_to_goal : t -> bool *)

  (* (\** {3 Modifiers} *\) *) 

  (** [set_block_index idx path_state] Set the block index of the path *)
  val set_block_index : int -> t -> t

  (** Jump to the specified virtual address *)
  val goto_vaddr : Virtual_address.t -> t -> t

  (** [assign value ps] Perform the update [value] on the symbolic
     store of [ps]. If [init=true] then this assignement is part of
     the initialization of the SE and not part of the real program. *)
  val assign: ?init:bool -> Relse_utils.assignment_t -> t -> t

  (* (\** Add the conditional at current location to the CFG *\)
   * val add_conditional_to_cfg: true_target:int Dba.jump_target ->
   *   false_target:int -> t -> unit
   * 
   * (\** Add the conditional at current location to the CFG *\)
   * val add_djump_to_cfg: Bitvector.t list -> t -> unit
   * 
   * (\** Return the list of successors of the current block in the CFG  *\)
   * val next_targets_from_cfg: t -> Virtual_address.t list option *)
  
  (** [update_pc_cond r_expr ps] Update path state [ps] after a
      conditional with the relational condition [r_expr]. If [spectre
      mode] is set, the condition is put on hold until the speculative
      execution is stopped. Otherwise it is directly added to the path
      constraint. *)
  val update_pc_cond : ?checked:bool -> Formula.bl_term Rel_expr.t -> t -> t

  (** [update_pc_static r_expr ps] Update the path state [ps] with the
      relational consition [r_expr] after a dynamic jump *)
  val update_pc_dynamic : ?checked:bool -> Formula.bl_term Rel_expr.t -> t -> t

  val pop_ret: t -> Virtual_address.t * t
  val push_ret: Virtual_address.t -> t -> t

  (** Initializes the memory at address [addr] *)
  val with_init_mem_at: addr:Bitvector.t -> size:int -> t -> t

  (** [address_belongs_to_init addr p] Check if the address [addr]
      belongs to the initialized memory locations of the symbolic state *)
  val address_belongs_to_init: addr:Bitvector.t -> t -> bool

  val mark_sat : t -> t
  val mark_unsat : t -> t
  val mark_unknown : t -> t

  (** {2 Printers} *)

  (** [pp_path ps] Pretty print the current location of [ps] *)
  val pp_loc : Format.formatter -> t -> unit

  (** [pp_path ps path_number] Pretty print the path leading to [ps]*)
  val pp_path : t -> int -> unit


  (** { Spectre }  *)

  val get_regular_pc: t -> Formula.bl_term Relse_utils.formula_status
  val get_transient_pc: t -> Formula.bl_term Relse_utils.formula_status
  val maybe_retire: t -> t status_t
  val start_explicit_transient_path: retire_depth:int -> t -> t
  val start_haunted_path: retire_depth:int ->
    transient_cond:Formula.bl_term Rel_expr.t ->
    regular_cond:Formula.bl_term Rel_expr.t -> t -> t
  val speculative_mode: t -> Spectre.mode
  val start_transient_load: t -> retire_depth:int -> t
end
