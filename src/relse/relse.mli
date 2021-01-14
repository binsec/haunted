(** TODO: description *)

(* module State : sig
 *   type t
 * 
 *   val create : path:Relse_path.Path_state.t ->
 *                insecurity:Relse_insecurity.Insecurity_state.t -> t
 * 
 *   val path : t -> Relse_path.Path_state.t
 *   val insecurity : t -> Relse_insecurity.Insecurity_state.t
 *   val symbolic_state : t -> Relse_symbolic.State.t
 *   
 *   val set_path : t -> Relse_path.Path_state.t -> t
 *   val set_insecurity : t -> Relse_insecurity.Insecurity_state.t -> t
 * 
 *   (\** [on_path f state] Applies the function [f] on the [path] of [state] *\)
 *   val on_path : (Relse_path.Path_state.t -> Relse_path.Path_state.t) -> t -> t
 * 
 *   (\** [on_insecurity f state] Applies the function [f] on the [insecurity] of [state] *\)
 *   val on_insecurity :
 *     (Relse_insecurity.Insecurity_state.t -> Relse_insecurity.Insecurity_state.t) ->
 *     t -> t
 * end
 * 
 * 
 * (\** The environment of the symbolic execution instance.
 *     Basically, it contains the next paths to explore*\)
 * module Env : sig
 *   type t = private {
 *     (\** List of the next paths to explore *\)
 *     worklist : Stack.t;
 *     (\* mergepoints :
 *      *   ((int * Path_state.t) Dba_types.Caddress.Map.t) Basic_types.Int.Map.t; *\)
 *     (\** The number of paths explored (SAT) *\)
 *     paths : int;
 *   }
 * 
 *   (\** {2 Constructors} *\)
 * 
 *   (\** Initializes the environment of the symbolic execution from address.
 *       - Decode the initial instruction
 *       - Initializes the symbolic store and state
 *       - Set the initial path *\)
 *   val from_address :
 *     initialize_fun:(Relse_path.Path_state.t -> Relse_path.Path_state.t) ->
 *     entrypoint:Virtual_address.t -> t
 * 
 *   (\** Pop the next path on the worklist *\)
 *   val choose_path : t -> (t * State.t) option
 *   (\** Push a path on the worklist *\)
 *   val add_path : State.t -> t -> t
 *   (\* val merge_if_necessary : t -> Path_state.t -> t * Path_state.t option *\)
 * 
 *   (\** Increments the number of SAT paths *\)
 *   val incr_paths : t -> t
 *   (\** Returns the number of paths explored *\)
 *   val get_nb_paths : t -> int
 * end
 * 
 * (\** Check if the current address must be avoided / max depth is
 *     exceeded / the goal is attained *\)
 * module Terminator : sig
 *   type t = private {
 *     (\** Reached an address to be avoided or max depth *\)
 *     end_path   : Env.t -> Relse_path.Path_state.t -> bool;
 *     (\** Reached the goal *\)
 *     end_search : Env.t -> Relse_path.Path_state.t -> bool;
 *   }
 * 
 *   (\** {2 Constructor} *\)
 * 
 *   val dfs :
 *     goals:Virtual_address.Set.t ->
 *     avoids:Virtual_address.Set.t ->
 *     t
 * end *)


val run : unit -> unit
