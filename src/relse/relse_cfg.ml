(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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
(* Mutable tree for a CFG that keeps track of dynamic jump targets for
   transient executions *)
(* type t =
 *   | 
 *   | Djump of djump_node
 *   | Cond of cond_node
 * 
 * and node = {
 *   current_depth: int; 
 *   vaddr: Virtual_address.t;
 * }
 * 
 * and target = {
 *   address: Virtual_address.t;
 *   successor: t;
 * }
 * 
 * and cond_node = {
 *   c_node: node;
 *   mutable target_true: target;
 *   mutable taret_false: target;
 * }
 * 
 * and djump_node = { 
 *   d_node: node;
 *   mutable targets: target list option;
 * } *)

type end_block = {
  (* The address of the end of this basic block *)
  end_addr : Dba_types.Caddress.t;

  (* The list of successors of this basic block *)
  successors : basic_block list;
}

and basic_block = {
  (* The concrete address and depth of the beginning of this block *)
  start_addr : Dba_types.Caddress.t;

  mutable end_block : end_block option;
}

type t = basic_block

let mk_basic_block start_addr = {
  start_addr;
  end_block = None;
}

let create ~entrypoint = mk_basic_block entrypoint

let end_basic_block ~assert_unique ~addr ~successors t =
  match t.end_block with
  | Some end_block when assert_unique ->
    begin
      Logger.error "[CFG][end_basic_block] at address %a already set \
                    and not matching"
        Dba_types.Caddress.pp_base end_block.end_addr;
      assert false;
    end
  | Some end_block ->
    assert (Dba_types.Caddress.equal addr end_block.end_addr)
  | _ ->
    let end_block = {
        end_addr = addr;
        successors = (List.map
                        (fun target_address ->
                           Logger.debug ~level:9 "[CFG][end_basic_block] \
                             Starting at %a, ending at %a going to target %a"
                             Dba_types.Caddress.pp_base t.start_addr
                             Dba_types.Caddress.pp_base addr
                             Dba_types.Caddress.pp_base target_address;
                           mk_basic_block target_address)
                        successors)
      } in
    t.end_block <- Some end_block;

exception UnknownTarget

let choose_next_target addr t =
  match t.end_block with
  | None -> raise UnknownTarget
  | Some end_block ->
    Logger.debug ~level:9 "[CFG][choose_next_target] %a"
      Dba_types.Caddress.pp_base addr;
    match List.find_opt (fun bb -> Dba_types.Caddress.equal bb.start_addr addr)
            end_block.successors with
    | Some basic_block -> basic_block
    | None -> raise UnknownTarget

(* Returns the successors of the current basic block ending at address
   [addr] *)
let get_jump_targets ~addr t =
  match t.end_block with
  | None -> None
  | Some end_block ->
    (* Ensure that the current address matches the end of the current
       basic block *)
    if not @@ Dba_types.Caddress.equal end_block.end_addr addr then
      begin
        Logger.error "[CFG][get_jump_targets] end addresses %a and %a not matching"
          Dba_types.Caddress.pp_base end_block.end_addr Dba_types.Caddress.pp_base addr;
        assert false;
      end;
    Some (List.map (fun bb ->
        Dba_types.Caddress.to_virtual_address bb.start_addr)
        end_block.successors)

let pp _ _ =
  failwith "Not implemented"
