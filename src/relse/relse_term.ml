(* A transient value is associated with a boolean term (set to false
   when the value needs to be retired) *)
open Formula
type value_t = Rel_expr.rel_bv

module Transient = struct

  type retire_info = Never | Depth of int | Var of bl_var * int

  type t = {
    retire_info: retire_info;
    value: value_t;
  }

  (* Onlys check if the value is equal *)
  let equal t t' =
    Rel_expr.equals Formula.equal_bv_term t.value t'.value

  let hash t = Rel_expr.hash (fun bv -> bv.bv_term_hash) t.value

  let create ?(retire_depth=None) ?(retire_var=None) value =
    let retire_info = match retire_depth, retire_var with
      | None, None -> Never
      | Some depth, None -> Depth depth
      | Some depth, Some var -> Var (var, depth)
      | _ -> failwith "Invalid transient term: missing depth" in
    { retire_info; value; }

  let value t = t.value
  let set_value t value = { t with value }

  let retire_depth t =
    match t.retire_info with
    | Never -> None
    | Depth depth -> Some depth
    | Var (_, depth) -> Some depth

  let apply f t = { t with value = f t.value }

  let apply2 f t1 t2 =
    let retire_info =
      match t1.retire_info, t2.retire_info with
      | Never, Never -> Never
      | Never, retire_info
      | retire_info, Never -> retire_info
      (* Keep the smallest retire depth *)
      | Depth d1, Depth d2 -> Depth (min d1 d2)
      | Var (bl1, d1), Var (bl2, d2) ->
        let bl, d = if bl1 < bl2 then bl1, d1 else bl2, d2 in
        Var (bl, d)
      | _ -> failwith "Incompatible retire_info" in
    let value = f t1.value t2.value in
    { retire_info; value }

  let unsafe_get_blvar t =
    match t.retire_info with
    | Var (bl_var, depth) -> bl_var, depth
    | _ -> assert false

  let pp ppf t =
    let pp_retire ppf retire_info =
      match retire_info with
      | Never -> Format.fprintf ppf "{inf}"
      | Depth d -> Format.fprintf ppf "{%d}" d
      | Var (bl,d) ->
        Format.fprintf ppf "{%a,%d}"
          Formula_pp.pp_bl_term (Formula.mk_bl_var bl) d
    in
    Format.fprintf ppf "%a%a\n"
      (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) t.value
      pp_retire t.retire_info
end

module TTHashSet = Hashset.Make(Transient)

module TransientSet = struct
  
  type t = {
    regular: Transient.t;
    transient: TTHashSet.t;
  }

  let create ?(size=10) bv = {
    regular = Transient.create bv;
    transient = TTHashSet.create size;
  }

  let has_transient t =
    TTHashSet.cardinal t.transient > 0

  let regular t = Transient.value t.regular
  let transient t = t.transient

  let fail_if_transient t =
    if has_transient t then
      failwith "This value should not have transient parts"

  (* Updates the SelectValue.t if needed and return true, or returns
     false if the value has not been added (i.e. if the value was
     already in the set) *)
  let add_transient ~bl_term ~retire_depth r_value t =
    if Rel_expr.equals equal_bv_term (regular t) r_value
    then t
    else
      let tt = Transient.create ~retire_var:bl_term
          ~retire_depth:(Some retire_depth) r_value in
      if TTHashSet.mem t.transient tt
      then t
      else (TTHashSet.add t.transient tt; t)

  (* let get_bl_trans t = *)
    

  let map f t =
    let regular = Transient.apply f t.regular in
    let transient = TTHashSet.create @@ TTHashSet.cardinal t.transient in
    TTHashSet.iter (fun tt ->
        TTHashSet.add transient @@ (Transient.apply f tt)) t.transient;
    { regular; transient }

  let fold f t a =
    let f' tt a = f (Transient.value tt) a in
    let a = f' t.regular a in
    TTHashSet.fold f' t.transient a

  let cartesian f t1 t2 =
    let size = TTHashSet.cardinal t1.transient *
               TTHashSet.cardinal t2.transient in
    let f = Transient.apply2 f in
    let regular = f t1.regular t2.regular in
    let transient = TTHashSet.create size in

    (* Handle t1.regular *)
    TTHashSet.iter (fun t2_elem ->
        TTHashSet.add transient (f t1.regular t2_elem))
      t2.transient;

    (* Handle t1.transient *)
    TTHashSet.iter (fun t1_elem ->
        TTHashSet.add transient (f t1_elem t2.regular);
        TTHashSet.iter (fun t2_elem ->
            TTHashSet.add transient (f t1_elem t2_elem))
          t2.transient;
      ) t1.transient;

    { regular; transient }

  (* This is very very bad *)
  let cartesian3 f t1 t2 t3 =
    if not @@ has_transient t1 then
      let f = f (Transient.value t1.regular) in
      cartesian f t2 t3
      else if not @@ has_transient t2 then
        let f = fun x y -> f x (Transient.value t2.regular) y in
      cartesian f t1 t3
      else if not @@ has_transient t3 then
        let f = fun x y -> f x y (Transient.value t3.regular) in
      cartesian f t1 t2
      else failwith "TODO: Implement cartesian3 correctly"

  (* Transforms the set of values to a new ite expression *)
  let to_ite t =
    let append_to_ite tt term =
      let bl_var, _ = Transient.unsafe_get_blvar tt in
      let bl_term = Formula.mk_bl_var bl_var in
      Rel_expr.apply2 (Formula.mk_bv_ite bl_term) (Transient.value tt) term
    in
    TTHashSet.fold append_to_ite t.transient (Transient.value t.regular)

  let pp ppf t =
    Format.fprintf ppf "regular_result: %a\n"
      Transient.pp t.regular;
    TTHashSet.iter (fun tt ->
        Format.fprintf ppf "transient_results: %a\n"
          Transient.pp tt)
      t.transient
end
