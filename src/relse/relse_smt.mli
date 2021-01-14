module Path_state = Relse_path.Path_state

module Solver : sig
  
  (* val with_solver :
   *   Relse_stats.query_type ->
   *   Relse_path.Path_state.t -> (Solver.Session.t -> 'a) -> 'a option *)
      
  (* val default : 'a -> 'a option -> 'a *)

  (** [check_sat_with_asserts formulas ps] Check the satisfiability of
      the [formulas] in the context of [ps] (without adding the path
      constraint). *)
  val check_sat_formulas :
    retire_stl:bool -> Relse_stats.query_type -> Formula.bl_term list -> Path_state.t ->
    Formula.status * Path_state.t
                             
  (** [check_sat_pc ps] Check the satisfiability of the path
      constraint in [ps] *)
  val check_sat_pc :
    retire_stl:bool -> Path_state.t ->
    Formula.status * Path_state.t

  (** [get_model_formulas formulas ps] Get a model assigning values
      to symbolic inputs that exercises the path [ps] with the extra
      assertions specified in [formulas] *)
  val get_model_formulas : retire_stl:bool -> Formula.bl_term list -> Path_state.t -> Smt_model.t
                         
  (** [get_model_pc ps] Get a model assigning values to symbolic
      inputs that exercises the path [ps] *)
  val get_model_pc : retire_stl:bool -> Path_state.t -> Smt_model.t

  (** [enumerate_values n expr State.t] Returns a maximum of [n]
      values satisfying [expr] in [path_state] *)
  val enumerate_values :
    retire_stl:bool -> int -> Formula.bv_term -> Formula.bl_term -> Path_state.t ->
    Bitvector.t list * Path_state.t
end

module Translate : sig
  
  (** [expr symbolic_state e high] Returns the two formulas
      representing the evaluation of the expressions [e] on the state
      [symbolic_state] in the original program and its renamed version.
      If [e] uses undeclared variables, those variables are declared as
      high if [high=true] or low otherwise.  *)
  val expr :
    ?high:bool -> Path_state.t -> Dba.Expr.t -> Relse_term.TransientSet.t

  (** Same as expr but fails if the returned value is a set of
      transient values *)
  val expr_noset:
    ?high:bool -> Path_state.t -> Dba.Expr.t -> Relse_memory.value_t
  
  (** [assignment lval rval path_state] If [high=true], all the
     variables created by [rval] are high. If [init=true] then this
     assignement is part of the initialization of the SE and not part
     of the real program. *)
  val assignment: ?init:bool -> Dba.LValue.t -> Rel_expr.rel_bv ->
    Path_state.t -> Path_state.t
end
