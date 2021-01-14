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

type assignment_t =
  | Var of string * Size.Bit.t * Rel_expr.rel_bv (* name, value *)
  | Mem of Dba.size * Rel_expr.rel_bv * Rel_expr.rel_bv (* size, index, value *)

let word_size_bits () = Kernel_options.Machine.bits ()
                        |> Machine.Bitwidth.bitsize    (* 32 bits *)
let word_size_bytes () = Kernel_options.Machine.bits ()
                         |> Machine.Bitwidth.bytesize  (* 4 bytes *)

let bytesize_toint = Basic_types.Constants.bytesize |> Natural.to_int

let int_to_bitvector i = Bitvector.of_int ~size:(Size.Bit.to_int (word_size_bits ())) i

let dba_constant_from_int n =
  Dba_utils.Expr.constant_from_int
    ~size:(word_size_bits () |> Size.Bit.to_int )
    ~value:n

let is_loadable addr =
  let loader = Kernel_functions.get_img () in
  let address = Bitvector.value_of addr |> Bigint.int_of_big_int in
  match Loader_utils.find_section_by_address ~address loader with
  | None -> false
  | Some section ->
    let name = Loader.Section.name section in
    Basic_types.String.Set.mem name (Sse_options.LoadSections.get()) ||
    Sse_options.LoadROSections.get() &&
    Loader.Section.(has_flag Loader_types.Read section &&
                    not (has_flag Loader_types.Write section))

(* Detects if the instruction is a return *)
let is_return instruction =
  Dhunk.is_return (Instruction.hunk instruction) 

(* Detect if the instruction is a conditional jump *)
(* Heuristic to detect conditional jump: has multiple outer targets. *)
let is_conditional_jump instruction =
  let dhunk = Instruction.hunk instruction in
  Virtual_address.Set.cardinal (Dhunk.outer_jumps dhunk) > 1


let read_bitvector addr sz =
  let b = Buffer.create (2 * sz) in
  (* The loop below is little-endian *)
  let rec loop offset =
    if offset < 0 then
      let v = Bigint.big_int_of_string ("0x" ^ Buffer.contents b) in
      let bv = Bitvector.create v ((Natural.to_int Basic_types.Constants.bytesize) * sz) in
      Logger.debug ~level:5 "[read_bitvector] Reading image at addr=%a, size=%d. Result=%a"
        Bitvector.pp_hex addr sz Bitvector.pp_hex bv;
      bv
    else
      let off_bv = int_to_bitvector offset in
      let load_addr = Bitvector.add addr off_bv in
      let img = Kernel_functions.get_img () in
      let byte = Loader_utils.get_byte_at img load_addr in
      let byte_str = Format.sprintf "%02x" byte in
      Buffer.add_string b byte_str;
      loop (offset - 1)
  in loop (sz - 1)

let is_sse () =
  match Relse_options.SymbolicStore.get () with
  | Sse -> true
  | _ -> false

let is_self_composed () =
  match Relse_options.SymbolicStore.get () with
  | SelfComposed -> true
  | _ -> false

let is_relational () =
  match Relse_options.SymbolicStore.get () with
  | Relational -> true
  | _ -> false


(* Warning: works because image never changes *)
let get_main_symbol =
  let sym = ref None in
  (fun () ->
     match !sym with
     | None ->
       let img = Kernel_functions.get_img () in  
       Loader_utils.symbol_by_name ~name:"main" img
       |> Utils.unsafe_get_opt
     | Some s -> s)

let relse_dirname = "binsec_relse"

let temp_file =
  let n = ref 0 in
  fun () ->
    incr n;
    let tmpdir = Sse_options.SMT_dir.get () in
    let temp_dir = Filename.concat tmpdir relse_dirname in
    if not (Sys.file_exists temp_dir) then
      begin
        Logger.debug ~level:6 "Creating directory %s" temp_dir;
        Unix.mkdir temp_dir 0o700
      end;
    let suffix = Format.sprintf "_%d.smt2" !n in
    let prefix = "relse" in
    let filename = Filename.temp_file ~temp_dir prefix suffix in
    Logger.debug ~level:5 "Creating temporary %s" filename;
    filename

let mk_var_name basename idx =
  Format.sprintf "%s_%d" basename idx

type 'a formula_status =
  | Valid
  | Unsat
  | Sat of 'a
  | Unknown of 'a

let get_formula = function
  | Valid -> Formula.mk_bl_true
  | Unsat -> Formula.mk_bl_false
  | (Sat fml | Unknown fml) -> fml 

let solving_attempt fml =
  if Formula.(equal_bl_term (mk_bl_comp BlEqual fml mk_bl_true) mk_bl_true)
  then Valid
  else if Formula.(equal_bl_term (mk_bl_comp BlEqual fml mk_bl_false) mk_bl_true)
  then Unsat
  else Unknown fml

let update_pc r_cond current_pc =
  let cond =
    if Rel_expr.is_relational r_cond
    then Rel_expr.apply_lr Formula.mk_bl_and r_cond
    else Rel_expr.value r_cond
  in
  match current_pc with
  | Unsat -> Unsat (* This formula is Unsat forever :( *)
  | Valid -> solving_attempt cond
  | Sat old_pc | Unknown old_pc ->
    match solving_attempt cond with
    | Valid ->
      (* The new assertion is true, status doesn't change *)
      current_pc
    | Unsat ->
      (* The condition is False, status becomes Unsat *)
      Unsat
    | _ ->
      (* Try to solve the new pc *)
      let new_pc = Formula.mk_bl_and old_pc cond in
      solving_attempt new_pc

type comparison_type = Equal | Distinct | NotComparable

let compare_bv bv1 bv2 =
  if Formula.(equal_bl_term (mk_bv_comp BvEqual bv1 bv2) mk_bl_true)
  then Equal
  else if Formula.(equal_bl_term (mk_bv_comp BvDistinct bv1 bv2) mk_bl_true)
  then Distinct (* TODO: Not working... *)
  else NotComparable

module AddressSet = struct
  type t = {
    set: Virtual_address.t Hashset.t;
    name : string;
  }

  let size t = Hashset.cardinal t.set

  let create ~name = { set = Hashset.create 10000; name }

  let add vaddr t =
    Hashset.add t.set vaddr

  let find vaddr t =
    Hashset.mem t.set vaddr

  let pp_as_address_trace ppf t =
    if Hashset.cardinal t.set = 0 then ()
    else
      Hashset.iter (fun vaddr ->
          Format.fprintf ppf "@[<h>%a@]@ " Virtual_address.pp vaddr)
        t.set;
      Format.fprintf ppf "@]"

  let pp_address_trace_to_file t path_number =
    if Sse_options.AddressTraceFile.is_set () then
      let filename = (Sse_options.AddressTraceFile.get ()) ^ t.name ^ "_" ^
                     (string_of_int path_number) in
      Print_utils.pp_to_file ~filename pp_as_address_trace t
end

module AddressList = struct
  type t = {
    list: Dba_types.Statement.t Sequence.t;
    name : string;
  }

  let virtual_address stmt =
    let open Dba_types in
    Statement.location stmt |> Caddress.to_virtual_address

  let create ~name = { list = Sequence.empty; name }
  let extend i al =
    match Sequence.peek_front al.list with
    | None -> { al with list = Sequence.push_front i al.list }
    | Some stmt ->
      let last_vaddres = virtual_address stmt in
      let v = virtual_address i in
      if v <> last_vaddres then
        { al with list = Sequence.push_front i al.list }
      else
        al

  let find target al =
    let target = virtual_address target in
    let found addr result =
      let addr = virtual_address addr in
      result || (addr = target)
    in
    Sequence.fold_forward found al.list false

  let pp_as_address_trace ppf list =   
    match Sequence.peek_front list with
    | None -> ()
    | Some _ ->
      Sequence.iter_forward
        (fun i ->
           let v = virtual_address i in
           Format.fprintf ppf "@[<h>%a@]@ " Virtual_address.pp v
        )
        list;
      Format.fprintf ppf "@]"

  let pp_address_trace_to_file al path_number =
    if Sse_options.AddressTraceFile.is_set () then
      let filename = (Sse_options.AddressTraceFile.get ()) ^ al.name ^ "_" ^
                     (string_of_int path_number) in
      Print_utils.pp_to_file ~filename pp_as_address_trace al.list
end

module Spectre = struct

  let unresolved_mem_access rexpr current_depth =
    let max_spec_depth = Relse_options.SpeculativeWindow.get () in
    match Rel_expr.get_date rexpr with
    | Some date when current_depth < date + max_spec_depth -> true
    | _ -> false
end


(* Formula manipulation *)
module F = struct

  let word_size () = word_size_bits () |> Size.Bit.to_int

  (** Name of a simple variable in the (original/renamed) program *)
  let with_index name index = name ^ "_" ^ (string_of_int index)
  let initial name = name ^ "_i"
  let left name = name ^ "_l"
  let right name = name ^ "_r"

  let rel_name ~high name =
    if high
    then Rel_expr.mk_rel (left name) (right name)
    else Rel_expr.mk_simple name
  
  let mk_initial_name ~high name =
    rel_name ~high (initial name)

  (** [memory] manipulation *)
  let memory_name = "__memory"
  (* Do not remove unit, otherwise it evaluates word_size before
     options are set *)
  let memory_type () =
    Formula.ax_sort (word_size ()) bytesize_toint
      
  let current_mem ~high idx =
    let r_name = rel_name ~high memory_name in
    Rel_expr.apply (fun name -> with_index name idx) r_name
    
  (** [pc] manipulation *)
  let pc = "__pc"
  let current_pc = with_index pc

  (** [formula] manipulation *)      
  let var name =
    let open Formula in
    function
    | BlSort       -> BlVar (bl_var name)
    | BvSort i     -> BvVar (bv_var name i)
    | AxSort (i,j) -> AxVar (ax_var name i j) 

  let decl =
    let open Formula in
    function
    | BlVar v -> mk_bl_decl v []
    | BvVar v -> mk_bv_decl v []
    | AxVar v -> mk_ax_decl v []

  let def value var =
    let open Formula in
    match value.term_desc, var with
    | BlTerm value, BlVar v -> mk_bl_def v [] value
    | BvTerm value, BvVar v -> mk_bv_def v [] value
    | AxTerm value, AxVar v -> mk_ax_def v [] value
    | _ -> failwith "F.def has incompatible types"

  (* let term =
   *   let open Formula in
   *   function
   *   | BlVar v -> mk_bl_term (mk_bl_var v)
   *   | BvVar v -> mk_bv_term (mk_bv_var v)
   *   | AxVar v -> mk_ax_term (mk_ax_var v) *)

  let memory_term mem_name =
    Formula.(mk_ax_var (ax_var mem_name (word_size ()) bytesize_toint))

  let mk_bv var_type name =
    match var_type with
    | Formula.BvSort i ->
      Formula.(mk_bv_var @@ bv_var name i)
    | _ -> failwith "[F.mk_bv_term] only for bitvectors"

  let mk_bl name =
    Formula.(mk_bl_var @@ bl_var name)

  let is_const expr =
    let open Formula in
    match expr.bv_term_desc with
    | BvCst _ -> true

    | _ -> false

  let is_var expr =
    let open Formula in
    match expr.bv_term_desc with
    | BvFun (_, []) -> true
    | _ -> false

  let is_canonical expr =
    let open Formula in
    match expr.bv_term_desc with
    | BvCst _ -> true
    | BvFun (_, []) -> true
    | BvFun _ -> false
    | BvLet _ -> false
    | BvUnop (_, bv) when is_const bv || is_var bv -> true
    | BvUnop _-> false
    | BvBnop (_, bv1, bv2)
      when (is_const bv1 && (is_const bv2 || is_var bv2)) ||
           (is_const bv2 && (is_var bv1)) ->  true
    | BvBnop _ -> false
    | BvIte _ -> false
    | Select _ -> false

  let normalize_simple expr iv =
    if is_canonical expr then expr else iv

  let normalize_rel r_expr r_iv =
    Rel_expr.apply2 normalize_simple r_expr r_iv

  let normalize r_name r_value var_type =
    let r_iv = Rel_expr.apply (mk_bv var_type) r_name in
    let r_result = normalize_rel r_value r_iv in
    let pp_value = Rel_expr.to_string Formula_pp.print_bv_term r_value in
    let pp_iv = Rel_expr.to_string Formula_pp.print_bv_term r_iv in
    let pp_result = Rel_expr.to_string Formula_pp.print_bv_term r_result in
    Logger.debug ~level:8 "[Symbolic state][Normalize] Expr=%s -- \
                           Iv=%s --> %s\n" pp_value pp_iv pp_result;
    r_result

  (** Manipultaion of the formula *)
  let fml_add_entry fml entry =
    Formula.push_front entry fml

  let fml_assign var_type name value fml =
    let var = var name var_type in
    let definition = def value var in
    fml_add_entry fml (Formula.mk_define definition) 
end
