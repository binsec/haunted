type t

val init : unit -> t

type return_type =
  | Continue of Relse_path.Path_state.t
  | Skip of Relse_path.Path_state.t
  | Halt

(** [check ctx ps] Updates the stub context [ctx] and the path state
    [ps] according to the current instruction. *)
val check : t -> Relse_path.Path_state.t -> return_type

(** The empty list of stubs  *)
val empty: t
