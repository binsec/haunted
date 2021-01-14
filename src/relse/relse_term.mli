type value_t = Rel_expr.rel_bv

module Transient : sig
  type retire_info = Never | Depth of int | Var of Formula.bl_var * int
  type t = { retire_info : retire_info; value : value_t; }
  val equal : t -> t -> bool
  val hash : t -> int
  val create :
    ?retire_depth:int option ->
    ?retire_var:Formula.bl_var option -> value_t -> t
  val value : t -> value_t
  val set_value : t -> value_t -> t
  val retire_depth : t -> int option
  val apply : (value_t -> value_t) -> t -> t
  val apply2 : (value_t -> value_t -> value_t) -> t -> t -> t
  val unsafe_get_blvar : t -> Formula.bl_var * int
  val pp : Format.formatter -> t -> unit
end

module TTHashSet : sig
  type elt = Transient.t
  type t = Hashset.Make(Transient).t
  val create : int -> t
  val clear : t -> unit
  val copy : t -> t
  val add : t -> elt -> unit
  val remove : t -> elt -> unit
  val mem : t -> elt -> bool
  val cardinal : t -> int
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

module TransientSet : sig
  (* type t = { regular : value_t option ref; transient : BVSet.t; } *)
  type t
  val create : ?size:int -> value_t -> t
  val has_transient : t -> bool
  val fail_if_transient: t -> unit
  val regular: t -> value_t
  val transient: t -> TTHashSet.t
  
  (** [add_transient t bl_term retire_depth r_bv] Updates the set [t]
     with the transient value [r_bv] if needed and return true, or
     returns false if the value has not been added (i.e. if the value
     was already in the set) *)
  val add_transient: bl_term:Formula.bl_var option -> retire_depth:int -> value_t -> t -> t
  
  val map : (value_t -> value_t) -> t -> t
  val fold: (value_t -> 'a -> 'a) -> t -> 'a -> 'a
  
  val cartesian : (value_t -> value_t -> value_t) -> t -> t -> t
  val cartesian3 : (value_t -> value_t -> value_t -> value_t) -> t -> t -> t -> t
  
  val to_ite: t -> value_t
  val pp : Format.formatter -> t -> unit
end
