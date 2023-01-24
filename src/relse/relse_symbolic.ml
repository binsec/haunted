(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Relse_options
module Memory = Relse_memory
module F = Relse_utils.F

type value_t = Rel_expr.rel_bv
type index_t = Rel_expr.rel_bv
type constraint_t = Rel_expr.rel_pc

module VarStore :
sig
  type t
  val create : store_type:store_type -> t
  val declare : high:bool -> string -> Formula.sort -> Formula.formula -> t -> (Formula.formula * t)
  val load : t -> string -> Size.Bit.t -> value_t
  val assign : string -> Size.Bit.t -> value_t -> Formula.formula -> t -> (Formula.formula * t)
  val add_declarations : t -> Formula.formula -> Formula.formula
end = struct  
  module Env = struct
    
    type var_infos = {
      value: value_t;
      index: int; (* Index of intermediate variable in the symbolic formula *)
      var_type: Formula.sort;
    }

    module M = Basic_types.String.Map

    type t = {
      variables: var_infos M.t;
      
      mutable definitions : (bool * Formula.sort) M.t;
      (** Variables that are loaded before they are defined.
          (high * var_type)
          (Shared by states). *)
      
      canonical: bool;
      store_type: store_type;
      
    }

    let create ~canonical ~store_type =
      let variables = M.empty in
      let definitions = M.empty in
      { variables; definitions ; canonical; store_type; }

    let get_infos t name = M.find_opt name t.variables

    let next_index t name var_type =
      match get_infos t name with
      | Some infos when infos.var_type <> var_type ->
        failwith "Store.get_last_index with wrong type"
      | Some infos -> infos.index + 1
      | None -> 0

    let duplicate t = t.store_type == SelfComposed
    let duplicate high t = duplicate t || high

    let fml_declare store_type high var_type r_name r_value fml =
      (* Add the variable to the formula *)
      let fml_declare name fml =
        let decl = F.decl (F.var name var_type)  in
        F.fml_add_entry fml (Formula.mk_declare decl)
      in
      match store_type with
      | Sse when high ->
        failwith "Cannot declare high variable in Sse"
      | Sse ->
        fml_declare (Rel_expr.value r_name) fml
      | SelfComposed ->
        let fml = fml_declare (Rel_expr.left r_name) fml in
        if high then
          fml_declare (Rel_expr.right r_name) fml
        else
          let term_l = Formula.mk_bv_term (Rel_expr.left r_value) in
          F.fml_assign var_type (Rel_expr.right r_name) term_l fml
      | Relational -> Rel_expr.fold fml_declare r_name fml

    let declare high name var_type fml t =
      let initial_index = -1 in
      let duplicate = duplicate high t in
      let r_initial_name = F.rel_name ~high:duplicate name in
      let r_initial_value = Rel_expr.apply (F.mk_bv var_type) r_initial_name in
      (* Add the variables to the store *)
      let store_declare value =
        let var_info = { value; index=initial_index; var_type } in
        { t with variables=(M.add name var_info t.variables) }
      in
      let t = store_declare r_initial_value in
      (* Add the variable to the formula *)
      let fml_declare name fml =
        let decl = F.decl (F.var name var_type)  in
        F.fml_add_entry fml (Formula.mk_declare decl)
      in
      let fml = 
        match t.store_type with
        | Sse when high ->
          failwith "Cannot declare high variable in Sse"
        | Sse ->
          fml_declare (Rel_expr.value r_initial_name) fml
        | SelfComposed ->
          let fml = fml_declare (Rel_expr.left r_initial_name) fml in
          if high then
            fml_declare (Rel_expr.right r_initial_name) fml
          else
            let term_l = Formula.mk_bv_term (Rel_expr.left r_initial_value) in
            F.fml_assign var_type (Rel_expr.right r_initial_name) term_l fml
        | Relational -> Rel_expr.fold fml_declare r_initial_name fml
      in fml,t

    let declare_in_place ?(high=false) name var_type t = (* TODO merge with declare *)
      Logger.debug ~level:3 "[Symbolic state][declare_in_place] Store: adding %s" name;
      let duplicate = duplicate high t in
      (* Add the variable to definitions *)
      t.definitions <- M.add name (high, var_type) (t.definitions);
      (* Compute the value of the variable *)      
      F.mk_initial_name ~high:duplicate name |>
      Rel_expr.apply (F.mk_bv var_type)

    let add_declarations t fml =
      (* Add the variable declarations to the formula *)
      let add_decl name (high,var_type) fml =
        let duplicate = duplicate high t in
        let r_name = F.mk_initial_name ~high:duplicate name in
        let r_value = Rel_expr.apply (F.mk_bv var_type) r_name in
        fml_declare t.store_type high var_type r_name r_value fml      
      in
      Formula.append
        fml
        (M.fold add_decl t.definitions Formula.empty)

    let get_last_value t name var_type =
      match get_infos t name with
      | Some infos when infos.var_type <> var_type ->
        failwith "Store.get_last_value with wrong type"
      | Some infos -> Some infos.value
      | None -> None

    let load t name size =
      let var_type = Formula.bv_sort (Size.Bit.to_int size) in
      match get_last_value t name var_type with
      | Some rel_val -> rel_val
      | None -> declare_in_place name var_type t


    let assign name size r_val fml t = (* Simple name, relational value *)
      let var_type = Formula.bv_sort (Size.Bit.to_int size) in
      let index = next_index t name var_type in
      let duplicate = duplicate (Rel_expr.is_relational r_val) t in
      let r_name = F.rel_name ~high:duplicate name |>
                   Rel_expr.apply (fun name -> F.with_index name index) in
      
      (* Add the variable to the formula *)
      let r_term = Rel_expr.apply Formula.mk_bv_term r_val in
      let fml = Rel_expr.fold2 (F.fml_assign var_type) r_name r_term fml in
      
      (* Add to the variable map *)
      let r_value =
        if t.canonical then
          F.normalize r_name r_val var_type
        else
          Rel_expr.apply (F.mk_bv var_type) r_name
      in
      let infos = { value=r_value; index; var_type; } in
      let variables =  M.add name infos t.variables
      in fml, { t with variables }
  end

  type t = Env.t

  let create ~store_type =
    let canonical = Canonical.get () in
    (* match store_type with
     * | SelfComposed when canonical ->
     *   failwith "SelfComposed var-store with canonical form not implemented yet"
     * | Shadow when (not canonical) ->
     *   failwith "Shadow var-store VarStore without canonical form not implemented yet"
     * | _ -> (); *)
    Env.create ~canonical ~store_type
  
  let declare ~high name var_type fml t =
    Env.declare high name var_type fml t
  
  let load t name size =
    Env.load t name size
  
  let assign name size r_val fml t =
   Env.assign name size r_val fml t
   
  let add_declarations t fml =
    Env.add_declarations t fml  
end

module PC :
sig
  type t
  val create : store_type:store_type -> t
  val get_current_pc : t -> Formula.bl_term Relse_utils.formula_status
  val update : ?checked:bool -> constraint_t -> Formula.formula -> t -> t * Formula.formula
  val add_initial : Formula.formula -> Formula.formula
  val mark_sat : t -> t
  val mark_unsat :  t -> t
  val mark_unknown :  t -> t
end = struct

  let fml_assign pc_index term fml =
    F.fml_assign Formula.bl_sort (F.current_pc pc_index) term fml
  
  type t = {
    store_type : store_type;
    index : int;
    current_pc : Formula.bl_term Relse_utils.formula_status
  }

  let mark_sat t =
    let pc = match t.current_pc with
      | Relse_utils.Valid -> Relse_utils.Valid
      | Relse_utils.Unsat -> failwith "Unsat pc cannot become sat"
      | Relse_utils.Sat pc | Relse_utils.Unknown pc -> Relse_utils.Sat pc
    in { t with current_pc = pc }

  let mark_unsat t =
    let pc = match t.current_pc with
      | Relse_utils.Valid -> failwith "Valid formula cannot be unsat"
      | Relse_utils.Sat _ -> failwith "This pc is already set to Sat"
      | Relse_utils.Unsat | Relse_utils.Unknown _ -> Relse_utils.Unsat
    in { t with current_pc = pc }

  let mark_unknown t =
    let pc = match t.current_pc with
      | Relse_utils.Valid -> Relse_utils.Valid
      | Relse_utils.Unsat -> Relse_utils.Unsat
      | Relse_utils.Sat pc | Relse_utils.Unknown pc -> Relse_utils.Unknown pc
    in { t with current_pc = pc }

  let create ~store_type =
    let index = 0 in
    let store_type = store_type in
    let current_pc = Relse_utils.Valid in (* First pc is set ot true *)
    { index; store_type; current_pc }

  let mk_term pc =
    Formula.(mk_bl_var (bl_var (F.current_pc pc.index)))

  let get_current_pc pc =
    pc.current_pc
  
  let update ?(checked=false) rel_cond fml pc =
    if Rel_expr.is_relational rel_cond && Relse_utils.is_sse () then
      failwith "Cannot update Sse store with relational value";
    match Relse_utils.update_pc rel_cond pc.current_pc with
    | Relse_utils.Valid ->
      (* Pc is valid, do nothing *)
      { pc with current_pc = Relse_utils.Valid }, fml
    | Relse_utils.Unsat ->
      { pc with current_pc = Relse_utils.Unsat }, fml  
    | Relse_utils.Sat pc_term as new_pc ->
      if new_pc = pc.current_pc then pc, fml
      else
        (* Set new temporary variable for path predicate *)
        let pc = { pc with index = pc.index + 1} in
        let new_pc = Relse_utils.Sat (mk_term pc) in
        { pc with current_pc = new_pc },
        (fml_assign pc.index (Formula.mk_bl_term pc_term) fml)
    | Relse_utils.Unknown pc_term as new_pc ->
      if new_pc = pc.current_pc then pc, fml
      else
        (* Set new temporary variable for path predicate *)
        let pc = { pc with index = pc.index + 1} in
        let new_pc = if checked
          then Relse_utils.Sat (mk_term pc)
          else Relse_utils.Unknown (mk_term pc) in
        { pc with current_pc = new_pc },
        (fml_assign pc.index (Formula.mk_bl_term pc_term) fml)
         
  let add_initial fml =
    fml_assign 0 Formula.(mk_bl_term mk_bl_true) fml
end

(*
  Containts the list of untainted variables.
  An untainted variable is represented as the right side of the
  relational variable. When an occurence of the right variable is
  detected, it is replaced by its mapping (the left variable).
*)
module Untainting =
struct
  open Rel_expr
  open Formula

  module M = Formula.VarMap
  
  type t = Formula.var M.t

  let empty = M.empty

  let add v v' u =
    Logger.debug ~level:1 "[Symbolic state][untaint] Adding: %s |-> %s"
      (Formula_utils.var_name v) (Formula_utils.var_name v');
    M.add v v' u

  (* WARNING: check before use *)
  (* let untaint_bl u r_bl =
   *   match deduplicate r_bl with
   *   | Rel (bl1, bl2) ->
   *     let pp_bl = Rel_expr.to_string Formula_pp.print_bl_term r_bl in
   *     Logger.debug ~level:1 "[Symbolic state][untaint] %s" pp_bl;
   *     let u = match bl1.bl_term_desc, bl2.bl_term_desc with
   * 
   *       (\* (\\* BvComp *\\)
   *        * | BvComp (BvEqual,bv1,bv2), BvComp (BvEqual,bv1',bv2')
   *        *   when equal_bv_term bv2 bv2' ->
   *        *   (match Formula_utils.is_bv_var bv1, Formula_utils.is_bv_var bv1' with
   *        *    | Some v, Some v' -> add (BvVar v') (BvVar v) u (\\* We can deduce v  = v' *\\)
   *        *    | _ -> u
   *        *   )
   *        * | BvComp (BvEqual,bv1,bv2), BvComp (BvEqual,bv1',bv2')
   *        *   when equal_bv_term bv1 bv1'->
   *        *   (match Formula_utils.is_bv_var bv2, Formula_utils.is_bv_var bv2' with
   *        *    | Some v, Some v' -> add (BvVar v') (BvVar v) u (\\* We can deduce v  = v' *\\)
   *        *    | _ -> u
   *        *   )
   *        * | BvComp _, BvComp _ -> u *\)
   *       (\* Others *\)
   *         (\* | BlFun  of bl_var * term list
   *          * | BlLet  of def list * bl_term
   *          * | BlUnop of bl_unop * bl_term
   *          * | BlBnop of bl_bnop * bl_term * bl_term
   *          * | BlComp of bl_comp * bl_term * bl_term
   *          * | BvComp of bv_comp * bv_term * bv_term
   *          * | AxComp of ax_comp * ax_term * ax_term
   *          * | BlIte  of bl_term * bl_term * bl_term         *\)
   *       | _ -> failwith "[untaint_bl] Not implemented yet"
   *     in bl1, u
   *   | Simple bl -> bl, u *)

  let rec untaint_bv u r_bv =
    match Rel_expr.get_value (deduplicate r_bv) with
    | Rel (bv, bv') ->
      let pp_bv = Rel_expr.to_string Formula_pp.print_bv_term r_bv in
      Logger.debug ~level:1 "[Symbolic state][untaint] %s" pp_bv;
      (match bv.bv_term_desc, bv'.bv_term_desc with
       (* We found a variable *)
       | BvFun (v,[]), BvFun (v',[]) ->
         add (BvVar v') (BvVar v) u (* We can deduce v  = v' *)

       (* BvUnop *)
       | BvUnop (BvNot,bv), BvUnop (BvNot,bv')
       | BvUnop (BvNeg,bv), BvUnop (BvNeg,bv') ->
         untaint_bv u (Rel_expr.mk_rel bv bv')

       (* BvBnop *)

       (* BvSub *)
       | BvBnop (BvSub,bv1,bv2), BvBnop (BvSub,bv1',bv2')
         when equal_bv_term bv2 bv2' ->
         untaint_bv u (Rel_expr.mk_rel bv1 bv1')
       | BvBnop (BvSub,bv1,bv2), BvBnop (BvSub,bv1',bv2')
         when equal_bv_term bv1 bv1' ->
         untaint_bv u (Rel_expr.mk_rel bv2 bv2')

       (* BvAdd *)
       | BvBnop (BvAdd,bv1,bv2), BvBnop (BvAdd,bv1',bv2')
         when equal_bv_term bv2 bv2' ->
         untaint_bv u (Rel_expr.mk_rel bv1 bv1')
       | BvBnop (BvAdd,bv1,bv2), BvBnop (BvAdd,bv1',bv2')
         when equal_bv_term bv1 bv1' ->
         untaint_bv u (Rel_expr.mk_rel bv2 bv2')

       (* BvConcat *)
       | BvBnop (BvConcat,bv1,bv2), BvBnop (BvConcat,bv1',bv2')
         when bv1.bv_term_size = bv1'.bv_term_size
           && bv2.bv_term_size = bv2'.bv_term_size ->
         untaint_bv (untaint_bv u (Rel_expr.mk_rel bv1 bv1'))
           (Rel_expr.mk_rel bv2 bv2')
       | BvBnop (BvAdd,bv1,bv2), BvBnop (BvAdd,bv1',bv2')
         when equal_bv_term bv1 bv1' ->
         untaint_bv u (Rel_expr.mk_rel bv2 bv2')

       (* Can't say anything more *)
       | BvBnop (BvAdd,_,_), BvBnop (BvAdd,_,_)
       | BvBnop (BvOr,_,_), BvBnop (BvOr,_,_)
       | BvBnop (BvAnd,_,_), BvBnop (BvAnd,_,_)
       | BvBnop (BvCmp,_,_), BvBnop (BvCmp,_,_)
       | BvBnop (BvMul,_,_), BvBnop (BvMul,_,_)
       (* TODO: Multiplication rule only works when there is no
           overflow -> we need to add interval informations here *)
       | BvBnop (BvShl,_,_), BvBnop (BvShl,_,_)
       | BvBnop (BvLshr,_,_), BvBnop (BvLshr,_,_)
       | BvIte (_,_,_), BvIte (_,_,_)
       | Select _, Select _ -> u (* TODO: no untainting of memory *)
       (* | BvCst  of Bitvector.t
        * | BvFun  of bv_var * term list
        * | BvLet  of def list * bv_term
        * | BvUnop of bv_unop * bv_term
        * | BvBnop of bv_bnop * bv_term * bv_term
        * | BvIte  of bl_term * bv_term * bv_term *)

       | _ -> failwith "[untaint_bv] Not implemented yet")
    | Simple _ -> u
  
  let subst_bv u bv =
    let subst v bv =
      match M.find_opt v u with
      | Some v' ->
        Logger.debug ~level:1 "[Symbolic state][untaint] Substituting: %s |-> %s"
          (Formula_utils.var_name v) (Formula_utils.var_name v');
        let var_name = Formula_utils.var_name v in
        let r s = if s = var_name then Formula_utils.var_name v' else var_name in
        Formula_transformation.rename_bv_term r bv
      | None -> bv
    in
    let varset = Formula_utils. bv_term_variables bv in
    Formula.VarSet.fold subst varset bv

  let subst_bl u bl =
    let subst v bl =
      match M.find_opt v u with
      | Some v' ->
        Logger.debug ~level:1 "[Symbolic state][untaint] Substituting: %s |-> %s"
          (Formula_utils.var_name v) (Formula_utils.var_name v');
        let var_name = Formula_utils.var_name v in
        let r s = if s = var_name then Formula_utils.var_name v' else var_name in
        Formula_transformation.rename_bl_term r bl
      | None -> bl
    in
    let varset = Formula_utils.bl_term_variables bl in
    Formula.VarSet.fold subst varset bl

  (* let subst_ax u ax =
   *   let subst v ax =
   *     match M.find_opt v u with
   *     | Some v' ->
   *       Logger.debug ~level:1 "[Symbolic state][untaint] Substituting: %s |-> %s"
   *         (Formula_utils.var_name v) (Formula_utils.var_name v');
   *       let var_name = Formula_utils.var_name v in
   *       let r s = if s = var_name then Formula_utils.var_name v' else var_name in
   *       Formula_transformation.rename_ax_term r ax
   *     | None -> ax
   *   in
   *   let varset = Formula_utils.ax_term_variables ax in
   *   Formula.VarSet.fold subst varset ax *)

  
  let subst_rbv u r_bv =
    if Rel_expr.is_relational r_bv
      (* Substitute in the right side of the relational expression *)
    then Rel_expr.apply_right (subst_bv u) r_bv |> deduplicate
    else r_bv

  (* let subst_rax u r_ax =
   *   match r_ax with
   *   (\* Substitute in the right side of the relational expression *\)
   *   | Rel (ax_l,ax_r) -> Rel (ax_l,subst_ax u ax_r) |> deduplicate
   *   | _ -> r_ax *)

end

module State =
struct

  (* Store bl_vars to retire transient values at a given depth *)
  module DatedBLVarSet = Hashset.Make(
    struct
      type t = int * Formula.bl_var
      let equal (_,bl) (_,bl') = Formula.(equal_bl_term (mk_bl_var bl) (mk_bl_var bl'))
      let hash (_,bl) = Formula.((mk_bl_var bl).bl_term_hash)
    end)

  type t = {
    (* The current formula *)
    formula : Formula.formula;

    (* Symbolic state *)
    var_store : VarStore.t;
    memory: Memory.t;
    pc: PC.t;

    (* Hypothesis to add to the formula *)
    hypothesis : Formula.bl_term list;

    (* List of variables to untaint *)
    untainter : Untainting.t;

    (* List of booleans declarations as bl_vars and their retire depth *)
    transient_loads: DatedBLVarSet.t option;
  }

  let address_belongs_to_init ~addr t =
    Memory.address_belongs_to_init ~addr t.memory

  let init_mem_at ~addr ~size st =
    Memory.init_mem_at ~addr ~size st.memory;
    { st with pc = PC.mark_unknown st.pc }

  let create () =
    let option_string =
      begin match SymbolicStore.get () with
        | Sse -> "SSE"
        | SelfComposed -> "SelfComposed"
        | Relational -> "Relational"
      end
      ^ " - " ^
      begin if Canonical.get ()
        then "Canonical Form" else "Intermediate Variables"
      end
      ^ " - " ^
      begin match MemoryType.get () with
        | MemStd -> "Standard Memory"
        | MemList-> "ROW with List"
        | MemMap -> "ROW with Map" end
    in
    Logger.debug ~level:1 "[State.create] %s " option_string;
    let store_type = SymbolicStore.get () in
    let formula = Formula.empty |> PC.add_initial in
    let var_store = VarStore.create ~store_type in
    let memory = Memory.create ~store_type in
    let pc = PC.create ~store_type in
    let hypothesis = [] in
    let untainter = Untainting.empty in
    let transient_loads =
      match SpectreStl.get () with
      | Relse_options.NoSTL -> None
      | Relse_options.Explicit -> None
      | Relse_options.HauntedIte -> Some (DatedBLVarSet.create 100) in
    { formula; var_store; memory; pc; hypothesis; untainter; transient_loads; }


  (* { Transient loads } *)

  let add_transient_loads ~retire_stl ~current_depth transient_loads fml =
    match transient_loads with
    | None -> fml
    | Some declarations -> 
      let add_decl bl_var fml =
        let decl = Formula.mk_bl_decl bl_var [] in
        F.fml_add_entry fml (Formula.mk_declare decl)
      in
      let add_def_false bl_var fml =
        let def = Formula.mk_bl_def bl_var [] Formula.mk_bl_false in
      F.fml_add_entry fml (Formula.mk_define def)
      in
      let add_var (retire_depth, bl_var) fml =
        if retire_stl || retire_depth <= current_depth
        then add_def_false bl_var fml    (* Retired *)
        else add_decl bl_var fml         (* Not retired yet *)
      in
      Formula.append fml (DatedBLVarSet.fold add_var declarations Formula.empty)    

  (* Set retirement depths of all loads to 0 *)
  let fence t =
    let transient_loads =
    match t.transient_loads with
    | None -> None
    | Some decls ->
      let decls' = DatedBLVarSet.create
          (DatedBLVarSet.cardinal decls) in
        (DatedBLVarSet.iter
          (fun (_, bl_var) -> DatedBLVarSet.add decls' (0, bl_var))
          decls;
        Some decls')
    in
    (* Clear store buffer *)
    let memory = Relse_memory.fence t.memory in
    { t with transient_loads; memory }

  let substitute_transient_loads_r_bv ~current_depth t r_expr =
    match t.transient_loads with
    | None -> r_expr
    | Some declarations ->
      let set_to_false (retire_depth, bl_var) r_expr =
        if retire_depth <= current_depth then
          (let def = Formula.mk_bl_def bl_var [] Formula.mk_bl_false in
           let r_expr' = Rel_expr.apply (Formula_transformation.replace_bv_term def) r_expr in
           if not @@ Rel_expr.equals Formula.equal_bv_term r_expr r_expr' then
             Logger.debug ~level:5 "[Spectre-STL] Setting transient load %a to false: %a -> %a"
               Formula_pp.pp_bl_term (Formula.mk_bl_var bl_var)
               (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_expr
               (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_expr'; r_expr')
        else r_expr
      in
      DatedBLVarSet.fold set_to_false declarations r_expr

  let substitute_transient_loads_bl ~current_depth t expr =
    match t.transient_loads with
    | None -> expr
    | Some declarations ->
      let set_to_false (retire_depth, bl_var) expr =
        if retire_depth <= current_depth then
          (let def = Formula.mk_bl_def bl_var [] Formula.mk_bl_false in
           let expr' = Formula_transformation.replace_bl_term def expr in
           if not @@ Formula.equal_bl_term expr expr' then
             Logger.debug ~level:5 "[Spectre-STL] Setting transient load %a to false: %a -> %a"
               Formula_pp.pp_bl_term (Formula.mk_bl_var bl_var)
               Formula_pp.pp_bl_term expr
               Formula_pp.pp_bl_term expr'; expr')
        else expr
      in
      DatedBLVarSet.fold set_to_false declarations expr


  (* { Formula } *)

  let comment cmt t =
    let comment = Formula.mk_comment cmt in
    { t with formula = Formula.push_front comment t.formula }
  
  let formula ~retire_stl ~current_depth t =
    let add_hypothesis formula term =
      Formula.push_front_assert term formula in
    let fml = t.formula
              |> VarStore.add_declarations t.var_store
              |> Memory.add_declaration t.memory
              |> add_transient_loads ~retire_stl ~current_depth t.transient_loads
              |> Formula.push_front_comment "Making Hypothesis" in
    List.fold_left add_hypothesis fml t.hypothesis
  
  let add_assertion hypothesis t =
    Logger.debug ~level:1 "[Symbolic state][Assert] Adding hypothesis \
                           %a" Formula_pp.pp_bl_term hypothesis;
    let formula = F.fml_add_entry t.formula (Formula.mk_assert hypothesis)
    in { t with formula }
  

  (* { Variables } *)
       
  let declare high name var_type t =
    let formula, var_store =
      VarStore.declare ~high name var_type t.formula t.var_store
    in { t with formula; var_store }
    
  let declare_high name sort t =
    Logger.debug ~level:1 "[Symbolic state][declare] %s as High" name;
    declare true name sort t

  let declare_low name sort t =
    Logger.debug ~level:1 "[Symbolic state][declare] %s as Low" name;
    declare false name sort t

  let var_load  ~current_depth t name size =
    let r_val = VarStore.load t.var_store name size
                |> Untainting.subst_rbv t.untainter
                |> substitute_transient_loads_r_bv t ~current_depth in
    Logger.debug ~level:1 "[Symbolic state][var_load] %s (%a bits) -> %a"
      name Size.Bit.pp size (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_val;
    r_val 
      
  let var_assign t name size r_val =
    Logger.debug ~level:1 "[Symbolic state][var_assign] %s (%a bits) := %a"
      name Size.Bit.pp size (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_val;
    let formula, var_store =
      VarStore.assign name size r_val t.formula t.var_store
    in { t with formula; var_store }

  
  (* { Path Constraint } *)

  let path_constraint ~current_depth t =
    match PC.get_current_pc t.pc with
    | Relse_utils.Valid ->
      Logger.debug ~level:1 "[Symbolic state][path_constraint] Valid path_constraint";
      Relse_utils.Valid
    | Relse_utils.Unsat ->
      Logger.debug ~level:1 "[Symbolic state][path_constraint] Unsat path_constraint";
      Relse_utils.Unsat
    | Relse_utils.Sat pc ->
      let pc = Untainting.subst_bl t.untainter pc |>
               substitute_transient_loads_bl ~current_depth t in
      Logger.debug ~level:1 "[Symbolic state][path_constraint] result: Sat %a"
        Formula_pp.pp_bl_term pc; Relse_utils.Sat pc
    | Relse_utils.Unknown pc ->
      let pc = Untainting.subst_bl t.untainter pc |>
               substitute_transient_loads_bl ~current_depth t in
      Logger.debug ~level:1 "[Symbolic state][path_constraint] result: Unknown %a"
        Formula_pp.pp_bl_term pc; Relse_utils.Unknown pc
  
  let pc_update ?(checked=false) t r_cond =
    Logger.debug ~level:1 "[Symbolic state][pc_update] pc := pc /\\ %a"
     (Rel_expr.pp_rexpr Formula_pp.pp_bl_term) r_cond;
    let pc, formula = PC.update ~checked r_cond t.formula t.pc
    in { t with formula; pc; }

  let mark_sat t =
    { t with pc = PC.mark_sat t.pc }

  let mark_unsat t =
    { t with pc = PC.mark_unsat t.pc }

  let mark_unknown t =
    { t with pc = PC.mark_unknown t.pc }

  
  (* { Memory } *)

  let record_transient_loads load_value_set t =
    match t.transient_loads with
    | Some transient_loads ->
      let add tt =
        Relse_term.Transient.(match tt.retire_info with
            | Never | Depth _-> assert false
            | Var (bl_term, depth) -> DatedBLVarSet.add transient_loads (depth, bl_term))
      in
      Relse_term.TTHashSet.iter add (Relse_term.TransientSet.transient load_value_set)
    | None -> ()

  let memory_select ~vaddr ~current_depth t size r_index =
    let select_value = Memory.select ~vaddr ~current_depth t.memory ~size r_index in
    record_transient_loads select_value t;
    let subst r_bv = Untainting.subst_rbv t.untainter r_bv
                     |> substitute_transient_loads_r_bv ~current_depth t in
    let select_value = Relse_term.TransientSet.map subst select_value in
    Logger.debug ~level:1 "[Symbolic state][memory_select] mem@%a * %d -> %a"
      (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_index size
      Relse_term.TransientSet.pp select_value;
    match Relse_options.DynamicPht.get () with
    | Relse_options.Static -> select_value
    | Relse_options.Hybrid _  | Relse_options.FullyDynamic ->
      begin
        Logger.debug ~level:8 "[Spectre][Dynamic] marking memory select at depth %d" (current_depth);
        Relse_term.TransientSet.map (Rel_expr.set_date (Some current_depth)) select_value
      end
    

  let memory_store ~init ~vaddr ~current_depth ~retire_depth t size r_index r_value =
    Logger.debug ~level:1 "[Symbolic state][memory_store] mem@%a * %d := %a"
      (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_index size
      (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_value;
    (* if Relse_options.Untainting.get () && Rel_expr.is_relational r_index then
     *   raise (Rel_expr.Should_not_be_relational ("in update_mem: " ^ pp_index)); *)
    let formula, memory =
      Memory.store ~init ~vaddr ~current_depth ~retire_depth ~size r_index r_value t.formula t.memory
    in { t with formula; memory; }

  
  (* { Untainting } *)
  
  (* let untaint_bl r_bl st =
   *   if Relse_options.Untainting.get () then
   *     (\* Untainting option is set and the variable is relational *\) 
   *     let bl, untainter = Untainting.untaint_bl st.untainter r_bl in
   *     Rel_expr.mk_simple bl, { st with untainter }
   *   else r_bl, st             (\* Otherwise, do nothing *\) *)

  let untaint_bv r_bv st =
    if Relse_options.Untainting.get () then
      (* Untainting option is set and the variable is relational *) 
      { st with untainter = Untainting.untaint_bv st.untainter r_bv}
    else st (* Otherwise, do nothing *)
end
