(** The exit codes *)
type query_type = Exploration | Control_Insecurity | Memory_Insecurity
                | Insecurity | Model | Enum

module Status : sig
  type status = Violation | Max_Depth | Max_Paths | Unsupported_Syscall
              | Solver_Timeout | Timeout | EnumLimit | Interrupt | OOM
end

type t
  
val empty: t

val get_exit_code : unit -> int
val set_status : Status.status -> unit
val enum_limit_reached: unit -> unit
val add_transient_dynamic_jump: unit -> unit

val update_status : Formula.status -> Dba_types.Statement.t -> query_type -> unit
val add_query : float -> Formula.status -> query_type -> unit
val add_query_size : int -> unit

(** Get the list of insecure addresses *)
val get_insecurity_addresses: unit -> Relse_utils.AddressList.t

(** Updates the done/spared insecurity checks *)
val add_done_check : unit -> unit
val add_spared_check : unit -> unit

val add_instruction : Virtual_address.t -> unit
val add_dba_instruction : unit -> unit
val add_path : unit -> unit
val add_conditional : unit -> unit
val add_fork : unit -> unit
val add_resolved_fork : unit -> unit
val add_unresolved_fork : unit -> unit
val discard_path : unit -> unit
val add_merged_path : unit -> unit
val add_refused_merger : unit -> unit
val add_aborted_merger : unit -> unit
val set_start : unit -> unit

(** Getters  *)
val get_nb_paths: unit -> int

(* Spectre *)
val add_transient_path : unit -> unit
val add_transient_load : unit -> unit

val pp : Format.formatter -> t -> unit
val pp_csv : with_header:bool -> label:string -> fp:int -> dd:int -> untainting:int ->
  Format.formatter -> t -> unit

(** Print the statistics at the end of the execution*)
val print_stats : unit -> unit

(** Get current execution time *)
val get_time: unit -> float
