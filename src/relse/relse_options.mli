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

(** {Definition of command-line & programmatic options for RelSE} *)

include Cli.S

(** { Verbose options }  *)

module PrintModel : Cli.BOOLEAN
module StatFile : Cli.STRING_OPT
module StatPrefix : Cli.STRING

(** { Timeouts }  *)

module Timeout : Cli.INTEGER
module MaxPaths : Cli.INTEGER

(** { Optimizations } *)
                     
module FaultPacking : Cli.INTEGER
module Dedup : Cli.INTEGER
module Untainting : Cli.BOOLEAN
module LowDecl : Cli.BOOLEAN
module Canonical : Cli.BOOLEAN

type store_type =
  | Sse
  | SelfComposed
  | Relational
module SymbolicStore : Cli.GENERIC with type t = store_type

type memory_type =
  | MemStd
  | MemList
  | MemMap
module MemoryType : Cli.GENERIC with type t = memory_type

(** { Specification options } *)                    

module HighBytes : Cli.INTEGER_SET
module HighSymbols : Cli.STRING_SET

type leak_info =
  | HaltLeak    (** Halts at first leak *)
  | InstrLeak   (** Report leaky instructions (instructions are
                    reported only once) *)
  | UniqueLeaks (** Repors unique expressions that are leaked along a
                    path *)
(* | Quantified                 (\* Quantifies the amount of bytes/bis
   *                                 leaked per Program/Path/Instruction \
   *                                 Not implemented yet *\) *)
module LeakInfo : Cli.GENERIC with type t = leak_info

(** Spectre options *)
type spectre_pht =
  | NoPHT
  | Explicit        (* 4-way forking: 2 transient / 2 regular *)
  | ExplicitSmarter (* 4-way forking with check_sat before solve (reduce queries) *)
  | Haunted         (* Transient execution shadowing regular execution *)
module SpectrePht : Cli.GENERIC with type t = spectre_pht

type spectre_dynamic_pht =
  | Static          (* Speculation depth fixed to speculative window size *)
  | Hybrid of int   (* Speculation depth fixed to some constant when
                       conditonal does not depend from memory *)
  | FullyDynamic    (* Retire conditional when all corresponding
                       memory accesses are resolved *)
module DynamicPht : Cli.GENERIC with type t = spectre_dynamic_pht

module SpeculativeWindow : Cli.INTEGER

type spectre_stl = NoSTL
                 | Explicit        (* Forks at each load/store interleaving *)
                 | HauntedIte      (* ite encoding of load expressions *)
module SpectreStl : Cli.GENERIC with type t = spectre_stl
module StoreBuffer : Cli.INTEGER
module SpectreMOB  : Cli.BOOLEAN
