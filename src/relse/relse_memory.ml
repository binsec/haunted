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
open Formula
open Formula_utils

module F = Relse_utils.F

type value_t = Rel_expr.rel_bv
type index_t = Rel_expr.rel_bv

module BH = Bitvector.Collection.Htbl

type 'a select_t = Found of 'a | NotFound | Aborted | Aborted_at of Formula.ax_term

module type ENV =
sig
  type t
  val create : store_type:store_type -> t
  val select : t -> int -> index_t -> Formula.bv_term select_t Rel_expr.t
  val store : int -> index_t -> value_t -> Formula.formula -> t -> Formula.formula * t
end

module type MEMVAR =
sig
  type t
  val create : unit -> t
  val current_mem_term : t -> Formula.ax_term Rel_expr.t
  val init_mem_at : addr:Bitvector.t -> size:int -> t -> unit
  val address_belongs_to_init: addr:Bitvector.t -> t -> bool
  val store : size:int -> index_t -> value_t -> Formula.formula -> t -> (Formula.formula * t)
  val select : t -> size:int -> index_t -> value_t
  val select_from : Formula.ax_term Rel_expr.t -> t -> size:int -> index_t -> value_t
  val decl_init_mem : t -> Formula.formula -> Formula.formula
  val select_from_init_mem : t -> size:int -> Formula.bv_term Rel_expr.t -> Formula.bv_term Rel_expr.t
end
module type MEM =
sig
  type t
  val create : unit -> t
  val init_mem_at : addr:Bitvector.t -> size:int -> t -> unit
  val address_belongs_to_init: addr:Bitvector.t -> t -> bool
  val store : size:int -> index_t -> value_t -> Formula.formula -> t -> (Formula.formula * t)
  val lookup : t -> size:int -> Formula.bv_term Rel_expr.t -> value_t select_t
  val select : t -> size:int -> index_t -> value_t
  val decl_init_mem : t -> Formula.formula -> Formula.formula
end

module type STBUF =
sig
  type t
  val create : unit -> t
  val init_mem_at : addr:Bitvector.t -> size:int -> t -> unit
  val address_belongs_to_init: addr:Bitvector.t -> t -> bool
  val store : init:bool -> vaddr:Virtual_address.t -> current_depth:int -> retire_depth:int -> size:int -> index_t ->
    value_t -> Formula.formula -> t -> (Formula.formula * t)
  val select : vaddr:Virtual_address.t -> current_depth:int -> size:int -> index_t -> t -> Relse_term.TransientSet.t
  val decl_init_mem : t -> Formula.formula -> Formula.formula
  val fence : t -> t
end

(* Address and intervals *)
type address = {
  base : bv_term;
  delta : Bigint.t;
}

let default base = { base; delta = Bigint.zero_big_int }

let get_address bv =
  match bv.bv_term_desc with
  | BvCst bv ->
    let base = mk_bv_zeros (Bitvector.size_of bv) in
    let delta = Bitvector.value_of bv in
    { base; delta }
  | BvBnop (b,bv1,bv2) ->
    (match b with
     | BvAdd ->
       (match is_bv_cst bv1, is_bv_cst bv2 with
        | Some bv1, Some bv2 ->
          default (mk_bv_cst (Bitvector.add bv1 bv2))
        | Some bv1, None ->
          { base = bv2; delta = Bitvector.value_of bv1 }
        | None, Some bv2 ->
          { base = bv1; delta = Bitvector.value_of bv2 }
        | None, None -> default bv)
     | BvSub ->
       (match is_bv_cst bv1, is_bv_cst bv2 with
        | Some bv1, Some bv2 ->
          default (mk_bv_cst (Bitvector.sub bv1 bv2))
        | Some bv1, None ->
          { base = mk_bv_neg bv2; delta = Bitvector.value_of bv1 }
        | None, Some bv2 ->
          { base = bv1; delta = Bigint.minus_big_int (Bitvector.value_of bv2)}
        | None, None -> default bv)
     | _ -> default bv)
  | BvFun (_,_)
  | BvLet (_,_)
  | BvUnop (_,_)
  | BvIte (_,_,_)
  | Select (_,_,_) -> default bv

let no_result results = Array.for_all ((=) None) results
let r_no_result results =
  match Rel_expr.get_value results with
  | Rel_expr.Simple r -> no_result r
  | Rel_expr.Rel (r1,r2) -> no_result r1 && no_result r2

let get_result results =
  let bv = ref (results.(0)) in
  Array.iteri
    (fun i opt -> if i > 0 then
        match !bv with
        | None -> ()
        | Some bv1 ->
          match opt with
          | None -> bv := None
          | Some bv2 -> bv := Some (mk_bv_concat bv2 bv1))
    results; !bv

(* Get the result from an array of relational bv [results].
   Return:
   - None if [results] contains at least one None,
   - Some Simple bv if [results] contains no shadow expression,
   - Some Rel (bv,bv') if [results] contains at least one shadow expression,
*)
let r_get_result results =
  let bv = ref results.(0) in
  Array.iteri 
    (fun i opt -> if i > 0 then
        match !bv with
        | None -> ()
        | Some r_bv1 ->
          match opt with
          | None -> bv := None
          | Some r_bv2 -> bv := Some (Rel_expr.apply2 mk_bv_concat r_bv2 r_bv1))
    results; !bv

let mk_relational_result results =
  if not @@ Rel_expr.is_relational !results then
    let r = Rel_expr.value !results in
    results := Rel_expr.(mk_rel r (Array.copy r))

type update_results_t = Match of value_t | Distinct | Overlap

(* update [result] with [bv] if the location at [address] and size [m] matches
   [address'] and size [n]*)
let rel_update_results results address m address' n r_bv =
  let size = (Rel_expr.left r_bv).bv_term_size / n in
  let open Bigint in
  if eq_big_int address.delta address'.delta && m = n && no_result (Rel_expr.left !results)
  then Match r_bv (* perfect match *)
  else begin
    let delta_m = pred_big_int (add_int_big_int m address.delta) in
    let delta_n = pred_big_int (add_int_big_int n address'.delta) in
    if lt_big_int delta_m address'.delta ||
       lt_big_int delta_n address.delta
    then Distinct (* no interval intersection *)
    else
      begin
        if Rel_expr.is_relational r_bv then mk_relational_result results;
        let max_big = min_big_int delta_m delta_n in
        let min_big = max_big_int address.delta address'.delta in
        let base_m  = sub_big_int min_big address.delta |> int_of_big_int in
        let base_n  = sub_big_int min_big address'.delta |> int_of_big_int in
        let loop = (sub_big_int max_big min_big |> int_of_big_int) in
        let update i results bv = 
          if results.(i + base_m) = None then
            let lo = (base_n + i) * size in
            let hi = (base_n + i + 1) * size - 1 in
            let bv = mk_bv_extract Interval.{lo; hi} bv in
            results.(i + base_m) <- Some bv;
            Logger.debug ~level:9 "[Symbolic memory][update_results] \
                                   results[%d] <- %s"
              (i + base_m) (Formula_pp.print_bv_term bv)
        in
        for i = 0 to loop do
          let _ = Rel_expr.apply2 (update i) !results r_bv in ()
        done;
        Rel_expr.(match get_value (apply get_result !results) with
            | Rel (Some bv, Some bv') -> Match (mk_rel bv bv')
            | Rel (None, None) -> Overlap
            | Simple (Some bv) -> Match (mk_simple bv)
            | Simple None -> Overlap
            | _ -> failwith "Invalid case")
      end
  end

(* let get_interval t bv =
 *   match is_bv_cst bv with
 *   | Some bv -> Interval.BitVecFlat.equal bv
 *   | None ->
 *     try BvTermHashtbl.find t bv
 *     with Not_found -> Interval.BitVecFlat.top (bv_size bv) *)

(* let set_interval t bv itv =
 *   get_interval t bv
 *   |> Interval.BitVecFlat.inter itv
 *   |> BvTermHashtbl.replace t bv *)


(** { Environments } *)

module InitMem = struct
  (* Table of of initial memory locations to read, shared between memories *)
  type t = {
    initialisation : int BH.t;  (* bitvector * size *)
    init_mem : (Formula.ax_term option) ref;
  }

  let create () = {
    initialisation = BH.create 100;
    init_mem = ref None;
  }

  let init_mem_at t ~addr ~size =
    match BH.find_opt t.initialisation addr with
    | Some size' when size <= size' ->
      Logger.debug ~level:3 "[Symbolic memory][init_mem_at] Already in memory %s<%d>"
        (Bitvector.to_hexstring addr) size;
    | _ ->
        Logger.debug ~level:3 "[Symbolic memory][init_mem_at] Add %s<%d>"
          (Bitvector.to_hexstring addr) size;
        BH.replace t.initialisation addr size

  let init_mem_name () =F.current_mem ~high:(Relse_utils.is_self_composed ()) 0 

  let decl_init_mem t fml =
    (* Declaration of __memory *)
    let mem_var = F.(var memory_name (F.memory_type ())) in
    let mem_declaration = F.decl mem_var in

    (* Definition of mem_0 *)
    let load_at addr size mem =
      assert (F.word_size () = Bitvector.size_of addr);
      let bv = mk_bv_cst (Relse_utils.read_bitvector addr size) in
      mk_store size mem (mk_bv_cst addr) bv
    in
    let mem_term = F.memory_term F.memory_name in
    let init_mem_term = mk_ax_term (BH.fold load_at t.initialisation mem_term) in

    let mem_name_0 = init_mem_name () in
    let add_mem_def name fml =
      let mem_def = F.var name (F.memory_type ()) |> F.def init_mem_term in
      Formula.push_back_define mem_def fml in

    (* Adding declarations to formula *)
    fml
    |> Rel_expr.fold add_mem_def mem_name_0
    |> Formula.push_back_declare mem_declaration


  exception Found
  let address_belongs_to_init ~addr t =
    let add_int bv n =
      let size = Bitvector.size_of bv in
      let bv_n = Bitvector.of_int ~size n in
      Bitvector.add bv bv_n in
    try
      Bitvector.Collection.Htbl.iter
        (fun kaddr vsize ->
           if
             Bitvector.compare addr kaddr >= 0 &&
             let end_addr = add_int kaddr vsize in
             Bitvector.compare addr end_addr < 0
           then raise Found
        ) (t.initialisation);
      false
    with Found -> true
end

(* Make logical select from the memory variable *)
let select_from_memory size memory_term index_term init_mem =
  match Formula_utils.is_bv_cst index_term with
  (* There might be a big chance that the select is from the initial memory *)
  (* If the select is concrete, we can add this address to the
     initial address to load *)
  | Some bv when Relse_utils.is_loadable bv ->
    Logger.debug ~level:9 "[Symbolic memory][memory_select] \
                           Aborted / added %a to init_mem_at\n"
      Bitvector.pp bv;
    InitMem.init_mem_at ~addr:bv ~size:size init_mem;
    Formula.mk_select size memory_term index_term 
  | _ ->
    (* Else, make select from last memory *)        
    Logger.debug ~level:9 "[Symbolic memory][memory_select] Aborted\n";
    Formula.mk_select size memory_term index_term

let rel_select_from_memory size r_memory r_index init_mem =
  Rel_expr.apply2 (fun memory_term index_term ->
      select_from_memory size memory_term index_term init_mem)
    r_memory r_index
  |> Rel_expr.deduplicate


(** The array variable representing the symbolic memory in the formula *)
module MemVar : MEMVAR = struct

  type t = {
    duplicated : bool; (* Is the memory duplicated in the formula ? *)
    mem_index : int;   (* Current index of the memory in the formula *)
    init_mem : InitMem.t
  }

  let create () = {
    mem_index = 0;
    duplicated = (Relse_utils.is_self_composed ());
    init_mem = InitMem.create ();
  }

  let current_mem_name t = F.current_mem ~high:t.duplicated t.mem_index
  let current_mem_term t = Rel_expr.apply F.memory_term (current_mem_name t)

  let init_mem_at  ~addr ~size t = InitMem.init_mem_at ~addr ~size t.init_mem

  let address_belongs_to_init ~addr t =
    InitMem.address_belongs_to_init ~addr t.init_mem

  let select_from_init_mem t ~size r_index =
    let aux init_mem_term index_term =
      (* Select from initial memory *)
      match Formula_utils.is_bv_cst index_term with
      (* If not found, and the address is concrete, select from
           initial memory *)
      | Some bv when Relse_utils.is_loadable bv ->
        let value = Relse_utils.read_bitvector bv size |>
                    Formula.mk_bv_cst in
        Logger.debug ~level:9 "[Symbolic memory][memory_select] Found in initial memory: %a\n"
          Formula_pp.pp_bv_term value;
        InitMem.init_mem_at ~addr:bv ~size:size t.init_mem;
        value
      | _ ->
        (* Else, make a select from initial memory *)
        Logger.debug ~level:9 "[Symbolic memory][memory_select] NotFound\n";
        Formula.mk_select size init_mem_term index_term
    in
    let init_mem_term = Rel_expr.apply F.memory_term (InitMem.init_mem_name ()) in
    Rel_expr.apply2 aux init_mem_term r_index |> Rel_expr.deduplicate        

  let select t ~size r_index =
    rel_select_from_memory size (current_mem_term t) r_index t.init_mem

  let select_from r_mem_term t ~size r_index =
    rel_select_from_memory size r_mem_term r_index t.init_mem  

  let store ~size r_index r_val fml t =
    let old_mem = current_mem_term t in
    let mem_index = t.mem_index + 1 in
    let duplicated = t.duplicated ||
                     Rel_expr.is_relational r_index ||
                     Rel_expr.is_relational r_val in
    if (duplicated && Relse_utils.is_sse ()) then
      failwith "Cannot duplicate memory in SSE";
    let t = { t with mem_index; duplicated; } in
    let new_mem = current_mem_name t in  
    let proj_store p fml =
      let open Rel_expr in
      let old_mem = proj p old_mem in
      let new_mem = proj p new_mem in
      let index = proj p r_index in
      let value = proj p r_val in 
      let value = mk_ax_term (mk_store size old_mem index value) in
      F.fml_assign (F.memory_type ()) new_mem value fml
    in
    if t.duplicated
    then fml |> proj_store Rel_expr.Left |> proj_store Rel_expr.Right, t
    else proj_store Rel_expr.Value fml, t

  let decl_init_mem t fml = InitMem.decl_init_mem t.init_mem fml
end

(** The array variable representing the symbolic memory in the formula *)
module DummyMemVar : MEMVAR = struct
  type t = unit
  let zero = Rel_expr.mk_simple Formula.mk_bv_zero
  let create () = ()
  let current_mem_name _ = F.current_mem ~high:false 0
  let current_mem_term _ = Rel_expr.apply F.memory_term (current_mem_name ())
  let init_mem_at ~addr:_ ~size:_ _ = ()
  let address_belongs_to_init ~addr:_ _ = false
  let select_from_init_mem _ ~size:_ _ = zero
  let select _ ~size:_ _ = zero
  let select_from _ _ ~size:_ _ = zero
  let store ~size:_ _ _ fml _ = fml, ()
  let decl_init_mem _ fml = fml
end

let initial_memory = F.(with_index memory_name 0)

module ListEnv : ENV =
struct
  type node =
    | Simple of (int * Formula.bv_term * Formula.bv_term)
    (* Simple index and simple value: mem[x] = <e> *)
    | RelValue of (int * Formula.bv_term * value_t)
    (* Simple index and relational value: mem[x] = <e|e'> *)
    | RelIndex of (int * index_t * value_t)
    (* Relational index and relational value: mem[x]|r = e and mem[x']|l = e'*)

  type t = {
    list: node list;
    mem_index: int;
    duplicated: bool;
    store_type: store_type;
  }

  let create ~store_type =
    { list=[]; mem_index=0; duplicated=(store_type == SelfComposed); store_type }

  let depth = ref max_int

  let current_mem_name t = F.current_mem ~high:t.duplicated t.mem_index
  let current_mem_term t = Rel_expr.apply F.memory_term (current_mem_name t)

  let abort t = Rel_expr.apply (fun name -> Aborted_at name) (current_mem_term t)

  let mk_node size index value =
    match Rel_expr.get_value index, Rel_expr.get_value value with
    | Rel_expr.Simple i, Rel_expr.Simple v -> Simple (size, i, v)
    | Rel_expr.Simple i, _ -> RelValue (size, i, value)
    | _, _ -> RelIndex (size, index, value)

  let simple_select t address size results fuel =
    let rec simple_select list address size results fuel =
      let get_results size' index' bv' list =
        let address' = get_address index' in
        if equal_bv_term address.base address'.base then
          match rel_update_results results address size address' size' bv' with
          | Match r -> Rel_expr.apply (fun x -> Found x) r
          | _ -> simple_select list address size results (fuel-1)
        else
          begin
            Logger.debug ~level:9 "[Symbolic memory][memory_select] \
                                   Aborted: %s not comparable with \
                                   %s\n"
              (Formula_pp.print_bv_term address.base)
              (Formula_pp.print_bv_term address'.base);
            abort t
          end
      in
      match list with
      | [] when r_no_result !results -> Rel_expr.mk_simple NotFound
      | _ when fuel <= 0 -> abort t
      (* We give up before the end of the list. TODO -> keep track of
         the memory on which we give up to search from it *)
      | Simple (size', index', bv') :: xs ->
        get_results size' index' (Rel_expr.mk_simple bv') xs
      | RelValue (size', index', r_bv') :: xs ->
        get_results size' index' r_bv' xs
      | _ -> failwith "Relational indexes in simple select -> TODO!"
    in simple_select t.list address size results fuel
  (* match status1, status2 with
   * | Equal, Equal -> Rel_expr.apply (fun x -> Found x) v
   * | Distinct, Distinct -> find_index mem' i
   * | _ ->
   *   let v1 = continue status1 Rel_expr.left
   *   and v2 = continue status2 Rel_expr.right
   *   in Rel_expr.mk_rel v1 v2 *)    

  let projected_select t address size results fuel p = 
    let abort = Aborted_at (Rel_expr.proj p (current_mem_term t)) in 
    let rec projected_select list address size results fuel =
      let get_results size' index' bv' list =
        let address' = get_address index' in
        let bv' = Rel_expr.mk_simple bv' in
        if equal_bv_term address.base address'.base then
          match rel_update_results results address size address' size' bv' with
          | Match bv -> Found (Rel_expr.value bv)
          | _ -> projected_select list address size results (fuel-1)
        else
          begin
            Logger.debug ~level:9 "[Symbolic memory][memory_select] \
                                   Aborted: %s not comparable with \
                                   %s\n"
              (Formula_pp.print_bv_term address.base)
              (Formula_pp.print_bv_term address'.base);
            abort
          end
      in
      match list with
      | [] -> NotFound
      | _ when fuel <= 0 -> abort
      (* We give up before the end of the list. TODO -> keep track of
         the memory on which we give up to search from it *)
      | Simple (size', index', bv') :: xs ->
        get_results size' index' bv' xs
      | RelValue (size', index', r_bv') :: xs ->
        get_results size' index' (Rel_expr.proj p r_bv') xs
      | RelIndex (size', r_index', r_bv') :: xs ->
        get_results size' (Rel_expr.proj p r_index') (Rel_expr.proj p r_bv') xs
    in projected_select t.list address size results fuel

  let select t size index =
    match Rel_expr.get_value index with
    | Rel_expr.Simple index ->
      let results = ref (Rel_expr.mk_simple (Array.init size (fun  _ -> None))) in
      let address = get_address index in
      simple_select t address size results !depth
    | Rel_expr.Rel _ ->
      if t.store_type == Sse then
        failwith "Cannot select relational index in SSE";
      let result_l = ref (Rel_expr.mk_simple (Array.init size (fun  _ -> None))) in
      let result_r = ref (Rel_expr.mk_simple (Array.init size (fun  _ -> None))) in
      let r_result = Rel_expr.mk_rel result_l result_r in
      let r_address = Rel_expr.apply get_address index in
      let projected_select p =
        let address = Rel_expr.proj p r_address in
        let result = Rel_expr.proj p r_result in
        projected_select t address size result !depth p
      in
      Rel_expr.(mk_rel (projected_select Left) (projected_select Right))

  let store size r_index r_val fml t =
    (* Update the list *)
    let list = mk_node size r_index r_val :: t.list in
    (* Update the formula *)
    let old_mem = current_mem_term t in
    let mem_index = t.mem_index + 1 in
    let duplicated = t.duplicated ||
                     Rel_expr.is_relational r_index ||
                     Rel_expr.is_relational r_val in
    if (duplicated && t.store_type == Sse) then
      failwith "Cannot duplicate memory in SSE";
    let store_type = t.store_type in
    let t = { list; mem_index; duplicated; store_type; } in
    let new_mem = current_mem_name t in
    let proj_store p fml =
      let open Rel_expr in
      let old_mem = proj p old_mem in
      let new_mem = proj p new_mem in
      let index = proj p r_index in
      let value = proj p r_val in 
      let value = mk_ax_term (mk_store size old_mem index value) in
      F.fml_assign (F.memory_type ()) new_mem value fml
    in
    if t.duplicated
    then fml |> proj_store Rel_expr.Left |> proj_store Rel_expr.Right, t
    else proj_store Rel_expr.Value fml, t

  (* let add_declaration _ ~init_mem fml =
   *   let mem_name = initial_memory in
   *   let mem_def = F.var mem_name (F.memory_type ()) |> F.def init_mem
   *   in Formula.push_back_define mem_def fml *)
end

module MapEnv : ENV =
struct

  module M = Basic_types.BigInt.Map

  type base_type =
    | NoBase
    | FirstBase of Formula.bv_term Rel_expr.t
    | SomeBase of Formula.bv_term Rel_expr.t

  type t = {
    map: value_t M.t;
    current_base: base_type;       (* The base of the last accessed index *)
    mem_index: int;                (* Current index of the memory in the formula *)
    duplicated: bool;              (* Is the memory duplicated in the formula ? *)
    store_type: store_type;        (* Type of the store: sse | self-composed | shadow *)
  }

  let create ~store_type = {
    map = M.empty;
    current_base = NoBase;
    mem_index = 0;
    duplicated = (store_type == SelfComposed);
    store_type
  }

  (* let depth = ref max_int *)

  let current_mem_name t = F.current_mem ~high:t.duplicated t.mem_index
  let current_mem_term t = Rel_expr.apply F.memory_term (current_mem_name t)

  let abort t = Rel_expr.apply (fun name -> Aborted_at name) (current_mem_term t)

  let base_equals addr base = equal_bv_term addr.base base
  let r_base r_addr = Rel_expr.apply (fun addr -> addr.base) r_addr
  let r_delta r_addr = Rel_expr.apply (fun addr -> addr.delta) r_addr

  let lookup t size r_addr not_found =
    let r_offset = r_delta r_addr |> Rel_expr.deduplicate in
    if Rel_expr.is_relational r_offset then
      failwith "Relational indexes not implemented in lookup of map environment";
    let offset = Rel_expr.value r_offset in
    let results = (Array.init size (fun i -> M.find_opt (Bigint.add_int_big_int i offset) t.map)) in
    match r_get_result results with
    | Some r_bv -> Rel_expr.apply (fun x -> Found x) r_bv
    | None ->
      if no_result results then not_found else (abort t)

  let select t size index =
    let debug_abort base  current_base =
      Logger.debug ~level:9 "[Symbolic memory][memory_select] Aborted: %s not comparable with %s"
        (Rel_expr.to_string (fun x -> Formula_pp.print_bv_term x.base) base)
        (Rel_expr.to_string Formula_pp.print_bv_term current_base)
    in
    let address = Rel_expr.apply get_address index in
    begin match t.current_base with
      | FirstBase current_base ->
        if Rel_expr.equals base_equals address current_base then
          (* When the offset is not found, select from initial memory *)
          let not_found = Rel_expr.mk_simple NotFound in
          lookup t size address not_found
        else
          (debug_abort address current_base; abort t)
      | SomeBase current_base ->
        if Rel_expr.equals base_equals address current_base then
          (* When the offset is not found, abort *)
          let not_found = abort t in
          lookup t size address not_found
        else
          (debug_abort address current_base; abort t)
      | NoBase ->
        (* Select form the initial memory *)
        Rel_expr.mk_simple NotFound
    end

  let store_update_fml size r_index r_val fml t = (* TODO merge parts with the ListEnv *)
    (* Update the formula *)
    let old_mem = current_mem_term t in
    let mem_index = t.mem_index + 1 in
    let duplicated = t.duplicated ||
                     Rel_expr.is_relational r_index ||
                     Rel_expr.is_relational r_val in
    if (duplicated && t.store_type == Sse) then
      failwith "Cannot duplicate memory in SSE";
    let store_type = t.store_type in
    let t = { t with mem_index; duplicated; store_type; } in
    let new_mem = current_mem_name t in
    let proj_store p fml =
      let open Rel_expr in
      let old_mem = proj p old_mem in
      let new_mem = proj p new_mem in
      let index = proj p r_index in
      let value = proj p r_val in 
      let value = mk_ax_term (mk_store size old_mem index value) in
      F.fml_assign (F.memory_type ()) new_mem value fml
    in
    if t.duplicated
    then fml |> proj_store Rel_expr.Left |> proj_store Rel_expr.Right, t
    else proj_store Rel_expr.Value fml, t

  let rec update map r_addr n bv =
    let r_offset = r_delta r_addr |> Rel_expr.deduplicate in
    if Rel_expr.is_relational r_offset then
      failwith "Relational indexes not implemented in update of map environment";
    let offset = Rel_expr.value r_offset in
    if n = 0 then map
    else
      let lo = (n-1) * Relse_utils.bytesize_toint in
      let hi = n * Relse_utils.bytesize_toint - 1 in
      let bv' = Rel_expr.apply (mk_bv_extract Interval.{lo; hi}) bv in
      let map' = M.add (Bigint.add_int_big_int (n-1) offset) bv' map in
      update map' r_addr (n-1) bv

  let store size r_index r_val fml t =
    let fml, t = store_update_fml size r_index r_val fml t in          
    let r_address = Rel_expr.apply get_address r_index in
    let map, current_base =
      match t.current_base with
      | FirstBase base | SomeBase base as current_base ->
        if Rel_expr.equals base_equals r_address base
        then
          (* Put the index in the current table *)
          let map = update t.map r_address size r_val in
          map, current_base
        else
          (* Create a new table *)
          let current_base = SomeBase (r_base r_address) in
          let map = update M.empty r_address size r_val in
          map, current_base
      | NoBase ->
        (* Create a new table with this base *)
        let current_base = FirstBase (r_base r_address) in
        let map = update M.empty r_address size r_val in
        map, current_base
    in fml, { t with current_base; map; }

  (* let add_declaration _ ~init_mem fml =
   *   let mem_name = initial_memory in
   *   let mem_def = F.var mem_name (F.memory_type ()) |> F.def init_mem
   *   in Formula.push_back_define mem_def fml *)
end

module RowMapEnv(MemVar : MEMVAR) : MEM = struct

  module M = Basic_types.BigInt.Map

  type base_type =
    | NoBase
    | FirstBase of Formula.bv_term
    | SomeBase of Formula.bv_term

  type t = {
    map: value_t M.t;
    current_base: base_type; (* The base of the last accessed index *)
    mem_var : MemVar.t
  }

  let create () = {
    map = M.empty;
    current_base = NoBase;
    mem_var = MemVar.create ();
  }

  let init_mem_at ~addr ~size t = MemVar.init_mem_at ~addr ~size t.mem_var

  let address_belongs_to_init ~addr t =
    MemVar.address_belongs_to_init ~addr t.mem_var

  let map_lookup t size addr =
    let offset = addr.delta in
    let results = (Array.init size (fun i -> M.find_opt (Bigint.add_int_big_int i offset) t.map)) in
    match r_get_result results with
    | Some r_bv -> Found r_bv
    | None -> if no_result results then NotFound else Aborted

  let base_equals addr base = equal_bv_term addr.base base

  let lookup t ~size r_index =
    (* Because at this point the index should be equal in both
       executions, we can project on the left component *)
    let address = Rel_expr.left r_index |> get_address in
    let abort msg current_base =
      Logger.debug ~level:9 "[Symbolic memory][memory_select][%s] Aborted \
                             at base %a (looking for %a)"
        msg Formula_pp.pp_bv_term address.base Formula_pp.pp_bv_term current_base;
      Aborted
    in
    match t.current_base with
    | FirstBase current_base when base_equals address current_base ->
      (* When the offset is not found, select from initial memory *)
      (match map_lookup t size address with
       | Found r_bv -> Found r_bv
       | Aborted | Aborted_at _ -> abort "NotFound" current_base
       | NotFound -> NotFound  (* Select form the initial memory *))

    | SomeBase current_base when base_equals address current_base ->
      (* When the offset is not found, abort *)
      (match map_lookup t size address with
       | Found r_bv -> Found r_bv
       | _ -> abort "NotFound" current_base)
    (* TODO: when base is distinct, selects from memory just before base. *)

    | SomeBase current_base
    | FirstBase current_base -> abort "NotComparable" current_base

    | NoBase -> NotFound (* Select form the initial memory *)


  let select t ~size r_index  =
    match lookup t ~size r_index with
    | Found r_bv -> r_bv
    | Aborted | Aborted_at _ ->
      (* Selects from current memory *)
      MemVar.select t.mem_var ~size r_index
    | NotFound ->
      (* Select form the initial memory *)
      MemVar.select_from_init_mem t.mem_var ~size r_index

  let rec update map addr n bv =
    let offset = addr.delta in
    if n = 0 then map
    else
      let lo = (n-1) * Relse_utils.bytesize_toint in
      let hi = n * Relse_utils.bytesize_toint - 1 in
      let bv' = Rel_expr.apply (mk_bv_extract Interval.{lo; hi}) bv in
      let map' = M.add (Bigint.add_int_big_int (n-1) offset) bv' map in
      update map' addr (n-1) bv

  let store ~size r_index r_val fml t =
    if Rel_expr.is_relational r_index then
      failwith "[store] Relational indexes not implemented in RowMap";
    let address = Rel_expr.value r_index |> get_address in
    let map, current_base =
      match t.current_base with
      | FirstBase base | SomeBase base as current_base
        when base_equals address base ->
        (* Put the index in the current table *)
        let map = update t.map address size r_val in
        map, current_base
      | FirstBase _ | SomeBase _ ->
        (* Create a new table *)
        let current_base = SomeBase address.base in
        let map = update M.empty address size r_val in
        map, current_base
      | NoBase ->
        (* Create a new table with this base *)
        let current_base = FirstBase address.base in
        let map = update M.empty address size r_val in
        map, current_base
    in
    (* Update the formula *)
    let fml, mem_var = MemVar.store ~size r_index r_val fml t.mem_var in
    fml, { current_base; map; mem_var; }

  let decl_init_mem t fml = MemVar.decl_init_mem t.mem_var fml
end


(* Memory with a symbolic store buffer *)
module StoreBuffer : STBUF = struct
  module RowMap = RowMapEnv(DummyMemVar)
  
  type node = {
    mem_term : Formula.ax_term Rel_expr.t;
    size : int;
    index : Formula.bv_term;
    value : value_t;
    retire_depth : int;
    vaddr : Virtual_address.t;
  }

  let mk_node ~size ~index ~value ~retire_depth ~vaddr ~mem_term =
    { mem_term; size; index; value; retire_depth; vaddr; }

  let pp_node ppf node =
    Format.fprintf ppf "{ idx=%a; val=%a; size=%d; retire_depth=%d; vaddr=%a }"
      Formula_pp.pp_bv_term node.index
      (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) node.value
      node.size node.retire_depth
      Virtual_address.pp node.vaddr

  type t = {    
    buf: node Sequence.t;
    main_mem : RowMap.t;
    main_mem_term : Formula.ax_term Rel_expr.t;
    mem_var : MemVar.t;
    size : int;
  }

  let create () =
    let mem_var = MemVar.create () in {
      size = Relse_options.StoreBuffer.get ();
      buf = Sequence.empty;
      main_mem = RowMap.create ();
      mem_var;
      main_mem_term = MemVar.current_mem_term mem_var;
    }

  let init_mem_at ~addr ~size t = MemVar.init_mem_at ~addr ~size t.mem_var

  let address_belongs_to_init ~addr t =
    MemVar.address_belongs_to_init ~addr t.mem_var

  let expired node current_depth =
    node.retire_depth <= current_depth

  let retire_last t =
    match Sequence.peek_back t.buf with
    | Some node ->
      Logger.debug ~level:9 "[Store Buffer][Retiring] %a\n" pp_node node;
      let index = Rel_expr.mk_simple node.index in
      let _,  main_mem = RowMap.store ~size:node.size index node.value Formula.empty t.main_mem in
      let main_mem_term = node.mem_term in
      { t with buf = Sequence.pop_back_ex t.buf; main_mem; main_mem_term }
    | None -> t

  let rec retire current_depth t =
    match Sequence.peek_back t.buf with
    | Some node when expired node current_depth ->
      let t = retire_last t in
      retire current_depth t
    | _ -> t

  let rec fence t =
    match Sequence.peek_back t.buf with
    | Some _ -> fence (retire_last t)
    | _ -> t

  let base_equals addr addr' = equal_bv_term addr.base addr'.base

  type result_t =  NotFound | PartiallyFound | Skip | Aborted | Found of value_t

  let select_node address size results buf =
    let rec aux first address size results buf = 
      match Sequence.peek_front buf with
      | Some node ->
        (Logger.debug ~level:9 "[Store Buffer][Comparing] with node %a\n" pp_node node;
         let node_address = get_address node.index in
         if base_equals address node_address then
           (* Comparabale indexes *)
           begin
             match rel_update_results results address size node_address node.size node.value with
             | Distinct when first ->
               (* This store node naturally comutes with the select *)
               Skip
             | Distinct | Overlap ->
               (* There is a hit but it is partial so we have to continue *)
               aux false address size results (Sequence.pop_front_ex buf)
             | Match r -> Found r
           end
         else
           begin
             Logger.debug ~level:9 "[Store Buffer][select_node] \
                                    Aborted: %a not comparable with %a\n"
               Formula_pp.pp_bv_term address.base
               Formula_pp.pp_bv_term node_address.base;
             Aborted
           end)
      | None when first ->
        (* We reached the end of the store bufer without finding a match. *)
        NotFound
      | None -> PartiallyFound
    in
    aux true address size results buf

  let lookup ~vaddr ~current_depth ~size r_index (t:t) =
    (* Because at this point the index should be equal in both
       executions, we can project on the left component *)
    let address = Rel_expr.left r_index |> get_address in
    let results = ref (Rel_expr.mk_simple (Array.init size (fun  _ -> None))) in 

    (* Get value from the main memory *)
    let main_mem_value () = match RowMap.lookup t.main_mem ~size r_index with
      | Aborted | Aborted_at _ ->
        Logger.debug ~level:1 "[StoreBuffer][select_main_mem] Aborted";
        MemVar.select_from t.main_mem_term t.mem_var ~size r_index
      | Found r_bv -> 
        Logger.debug ~level:1 "[StoreBuffer][select_main_mem] Found"; r_bv
      | NotFound -> 
        Logger.debug ~level:1 "[StoreBuffer][select_main_mem] NotFound";
        MemVar.select_from_init_mem t.mem_var ~size r_index in

    Logger.debug ~level:1 "[StoreBuffer][select] { idx=%a, size=%d, depth=%d }"
      Formula_pp.pp_bv_term (Rel_expr.left r_index) size current_depth;
    (* Logger.debug ~level:1 "[StoreBuffer][select] Current store buffer:";
     * Sequence.iter_backward (fun x ->
     *     Logger.debug ~level:1 "[StoreBuffer][select] %a"
     *       pp_node x) t.buf; *)

    let idx = ref 0 in
    let mk_bl_trans ~store_addr ~depth =
      let store_addr = match store_addr with
        | Some vaddr -> Virtual_address.to_string vaddr
        | None -> "main-mem"
      in
      let name = Format.sprintf "load_%s_from_%s_id-%d-%d"
          (Virtual_address.to_string vaddr) store_addr depth !idx in
      idx := !idx + 1;
      Formula.bl_var name
    in

    (* Update [results] with [r_value]. [r_value] should be retired at
       depth [retire_depth] *)
    let update_results results ~store_addr retire_depth r_value =
      match results with
      | None -> Some (Relse_term.TransientSet.create ~size:t.size r_value)
      | Some results ->
        (match retire_depth with
         | None -> assert false
         | Some retire_depth ->
           let bl_term = match SpectreStl.get () with
             | Relse_options.NoSTL | Relse_options.Explicit -> None
             | Relse_options.HauntedIte -> Some (mk_bl_trans ~store_addr ~depth:current_depth)
           in
           (* (\* Add bl_term to the list of declarations *\)
            * DatedBLVarSet.add declarations (retire_depth, bl_term); *)
           Some (Relse_term.TransientSet.add_transient ~bl_term ~retire_depth r_value results))
    in

    let rec process_next_node buf retire_depth select_results =
      match Sequence.peek_front buf with
      | Some node ->
        (match select_node address size results buf with
         | NotFound ->
           Logger.debug ~level:6 "[StoreBuffer][select] NotFound";
           update_results select_results ~store_addr:None
             retire_depth @@ main_mem_value ()

         | Skip -> 
           Logger.debug ~level:6 "[StoreBuffer][select] Skip node";
           process_next_node (Sequence.pop_front_ex buf) retire_depth select_results

         | PartiallyFound | Aborted ->
           Logger.debug ~level:6 "[StoreBuffer][select] Aborted";
           (* Could not resolve this load -> select from corresponding formula *)
           let select_results =
             update_results select_results ~store_addr:(Some node.vaddr) retire_depth @@
             MemVar.select_from node.mem_term t.mem_var ~size r_index in
           if not @@ expired node current_depth
           then process_next_node (Sequence.pop_front_ex buf) (Some node.retire_depth) select_results
           else select_results

         | Found bv ->
           Logger.debug ~level:6 "[StoreBuffer][select] Found";
           let select_results = update_results
               select_results ~store_addr:(Some node.vaddr) retire_depth bv in
           if not @@ expired node current_depth
           then process_next_node (Sequence.pop_front_ex buf) (Some node.retire_depth) select_results
           else select_results)
      | _ -> Logger.debug ~level:6 "[StoreBuffer][select] NotFound";
        update_results select_results ~store_addr:None retire_depth @@ main_mem_value () (* We can stop here *)
    in
    let select_results = process_next_node t.buf None None |> Utils.unsafe_get_opt in
    Logger.debug ~level:1 "[StoreBuffer][select] Results: \n %a"
      Relse_term.TransientSet.pp select_results;
    select_results

  let select ~vaddr ~current_depth ~size r_index (t:t) =
    lookup ~vaddr ~current_depth ~size r_index t

  let store ~init ~vaddr ~current_depth ~retire_depth ~size r_index r_val fml t =
    (* Printf.printf "TODO: DynamicSTL"; *)
    (* Updates the formula *)
    let fml, mem_var = MemVar.store ~size r_index r_val fml t.mem_var in

    (* Because indexes should be equal in both executions, we can
       project on the left side of the index *)
    let index = Rel_expr.left r_index in
    let mem_term = MemVar.current_mem_term mem_var in
    let node = mk_node ~mem_term ~size ~index ~value:r_val ~retire_depth ~vaddr in

    let t =
      if init && Sequence.length t.buf > 0 then
        failwith "Cannot init memory when StoreBuffer is not empty"
      else if init then
        (* If [init] then do not put the value in the store buffer. *)
        let index = Rel_expr.mk_simple node.index in
        let _,  main_mem = RowMap.store ~size:node.size index node.value Formula.empty t.main_mem in
        let main_mem_term = node.mem_term in
        { t with  main_mem; main_mem_term }
      else
        (* Else update the store buffer. *)
        (Logger.debug ~level:1 "[StoreBuffer][store] Storing %a in the \
                                store_buffer" pp_node node;          
         let buf = Sequence.push_front node t.buf in
         (* Logger.debug ~level:1 "[StoreBuffer][store] Current store buffer:";
          * Sequence.iter_backward (fun x ->
          *     Logger.debug ~level:1 "[StoreBuffer][store] %a"
          *       pp_node x) t.buf; *)
         { t with buf })
    in

    (* Retire expired stores *)
    let t = retire current_depth t in
    let t = if Sequence.length t.buf >= t.size
      then retire_last t else t in
    fml, { t with mem_var }

  let decl_init_mem t fml =
    MemVar.decl_init_mem t.mem_var fml
end


module Packer = struct
  module RowMap = RowMapEnv(MemVar)
  type pack =
    | Std of MemVar.t
    | List  of ListEnv.t
    | Map of MapEnv.t
    | RowMap of RowMap.t
    | SBuf of StoreBuffer.t

  type t = {
    pack : pack;
    store_type : store_type;
    init_mem : InitMem.t;
  }

  let init_mem_at ~addr ~size t = 
    match t.pack with
    | Std env -> MemVar.init_mem_at ~addr ~size env
    | List _ | Map _ -> InitMem.init_mem_at ~addr ~size t.init_mem
    | RowMap env -> RowMap.init_mem_at ~addr ~size env
    | SBuf env -> StoreBuffer.init_mem_at ~addr ~size env

  let address_belongs_to_init ~addr t = 
    match t.pack with
    | Std env -> MemVar.address_belongs_to_init ~addr env
    | List _ | Map _ -> InitMem.address_belongs_to_init ~addr t.init_mem
    | RowMap env -> RowMap.address_belongs_to_init ~addr env
    | SBuf env -> StoreBuffer.address_belongs_to_init ~addr env

  let create ~store_type =
    let spectre_stl = Relse_options.SpectreStl.get () in
    let ensures_nostl () = if spectre_stl <> Relse_options.NoSTL then
        failwith "SpectreSTL not compatible with current memory type" in
    let pack = match MemoryType.get () with
      | MemStd ->
        ensures_nostl (); Std (MemVar.create ())
      | _ when store_type = SelfComposed ->
        failwith "ROW with SelfComp store not implemented yet"
      | MemList -> List (ListEnv.create ~store_type)
      (* | MemMap -> Map (MapEnv.create ~store_type) *)
      | MemMap ->
        (match spectre_stl with
         | Relse_options.NoSTL -> RowMap (RowMap.create ())
         | Relse_options.Explicit -> SBuf (StoreBuffer.create ())
         | Relse_options.HauntedIte -> SBuf (StoreBuffer.create ()))
    in
    { pack; store_type; init_mem = InitMem.create (); }

  let store ~init ~vaddr ~current_depth ~retire_depth ~size r_index r_value fml t =
    let fml, pack = match t.pack with
      | Std env ->
        let fml, env = (MemVar.store ~size r_index r_value fml env)
        in fml, Std env
      | List env -> 
        let fml, env = (ListEnv.store size r_index r_value fml env)
        in fml, List env 
      | Map env -> 
        let fml, env = (MapEnv.store size r_index r_value fml env)
        in fml, Map env 
      | RowMap env -> 
        let fml, env = (RowMap.store ~size r_index r_value fml env)
        in fml, RowMap env 
      | SBuf env -> 
        let fml, env = (StoreBuffer.store ~init ~vaddr ~current_depth
                          ~retire_depth ~size r_index r_value fml env)
        in fml, SBuf env 
    in fml, { t with pack }

  let select t ~vaddr ~current_depth ~size r_index =
    let mk_select index_term mem_term =
      Formula.mk_select size mem_term index_term
    in     
    let process_value value index_term =
      match value with
      | Found value ->
        (* If found, return the value *)
        Logger.debug ~level:9 "[memory_select] Found %s\n" (Formula_pp.print_bv_term value);
        value
      | NotFound -> (* Select from initial memory *)
        begin
          match Formula_utils.is_bv_cst index_term with
          (* If not found, and the address is concrete, select from
             initial memory *)
          | Some bv when Relse_utils.is_loadable bv ->
            let value = Relse_utils.read_bitvector bv size |>
                        Formula.mk_bv_cst in
            Logger.debug ~level:9 "[Symbolic memory][memory_select] Found in initial memory: %a\n"
              Formula_pp.pp_bv_term value;
            InitMem.init_mem_at ~addr:bv ~size:size t.init_mem;
            value
          | _ ->
            (* Else, make a select from initial memory *)
            Logger.debug ~level:9 "[Symbolic memory][memory_select] NotFound\n";
            mk_select index_term (F.memory_term initial_memory)
        end
      | Aborted_at last_mem -> select_from_memory size last_mem index_term t.init_mem
      | Aborted -> failwith "Cannot process abort"
    in
    match t.pack with
    | Std env -> Relse_term.TransientSet.create (MemVar.select env ~size r_index)
    | List env ->
      let r_val = ListEnv.select env size r_index in
      Relse_term.TransientSet.create
        (Rel_expr.deduplicate @@ Rel_expr.apply2 process_value r_val r_index)
    | Map env ->
      let r_val = MapEnv.select env size r_index in
      Relse_term.TransientSet.create
        (Rel_expr.deduplicate @@ Rel_expr.apply2 process_value r_val r_index)
    | RowMap env -> Relse_term.TransientSet.create (RowMap.select env ~size r_index)
    | SBuf env -> StoreBuffer.select ~vaddr ~current_depth ~size r_index env

  let fence t =
    match t.pack with
      | Std _
      | List _
      | Map _
      | RowMap _ -> t
      | SBuf env ->
        { t with pack = SBuf (StoreBuffer.fence env) }


  let add_declaration t fml =
    (* Initialize the symbolic memory *)
    match t.pack with
    | Std env -> MemVar.decl_init_mem env fml
    | List _ | Map _ ->
      InitMem.decl_init_mem t.init_mem fml
    | RowMap env -> RowMap.decl_init_mem env fml
    | SBuf env -> StoreBuffer.decl_init_mem env fml
end
include Packer
