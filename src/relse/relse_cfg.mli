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

(** Mutable tree for a CFG that keeps track of dynamic jump targets for
    transient executions *)
type t

val create: entrypoint:Dba_types.Caddress.t -> t

val end_basic_block: assert_unique:bool -> addr:Dba_types.Caddress.t ->
  successors:Dba_types.Caddress.t list -> t -> unit

val choose_next_target: Dba_types.Caddress.t -> t -> t

(* Returns the successors of the current basic block *)
val get_jump_targets: addr:Dba_types.Caddress.t -> t -> Virtual_address.t list option

val pp: Format.formatter -> t -> unit
