type level = Path | Block | Instr

(** Module that handles all the insecurity checks.
    Contains all the checks related to a path. *)
module Insecurity_State :
sig

  type t

  (** Create a new insecurity state with no insecurity checks *)
  val create : unit -> t

  (** [add_memory_check ps instr is] Collects memory insecurity check
     of hte instruction [instr] under the symbolic state [ps] and add
     them to the insecurity state [is] *)
  val add_memory_check : Relse_path.Path_state.t -> Dba.Instr.t -> t -> t

  (** [add_memory_check r_expr is] Add a memory insecurity check to
     the insecurity state [is] to check [r_expr] cannot leak secret
     information *)
  val add_cf_check : Rel_expr.rel_bv -> t -> t

  (** [check_insecurity level is] Perform a check (according to the
      [level] and [fp] parameters) to ensure that no insecurity query is SAT *)
  val check_insecurity : level -> Relse_path.Path_state.t -> t -> (Relse_path.Path_state.t * t)
end
