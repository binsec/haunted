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

include Cli.Make(
struct
  let shortname = "relse"
  let name = "Relational symbolic execution"
end
)

module MaxPaths = Builder.Integer(
  struct
    let name = "paths"
    let default = 100
    let doc = "set maximal number of paths to explore"
  end
  )

module Timeout = Builder.Integer(
  struct
    let name = "timeout"
    let doc = "Set the overall timeout of the relse (seconds) (0 = infinite)"
    let default = 0
  end
  )

module StatFile = Builder.String_option(
  struct
    let name = "stat-file"
    let doc = "Write stats in csv format to this file"
  end
  )

module StatPrefix = Builder.String(
  struct
    let default = ""
    let name = "stat-prefix"
    let doc = "Label of the row in stat file"
  end
  )

module HighBytes = Builder.Integer_set(
  struct
    let name = "high-var"
    let doc = "Set the offset of the high bytes from esp.\n\t The \
               first [push ebp] is taken into account but\n\t if there \
               are other [push] instructions before [mov ebp, \
               esp],\n\t they must be included in the offset\n\t (add + 4 \
               for each push for a 32bit architecture)"
  end
  )

module HighSymbols = Builder.String_set(
  struct
    let name = "high-sym"
    let doc = "Name of symbols containing high data."
  end
  )

module PrintModel = Builder.False(
  struct
    let name = "print-model"
    let doc = "If enabled, prints the model satisfying the insecurity formula."
  end
  )

module FaultPacking = Builder.Integer(
  struct
    let default = 2
    let name = "fp"
    let doc = "Frequency of the insecurity check:\n" ^
               "\t\t- fp=0: None\n" ^
               "\t\t- fp=1: Instruction level\n" ^
               "\t\t- fp=2: Block level\n" ^
               "\t\t- fp=3: Path level"
  end
  )

module Dedup = Builder.Integer(
  struct
    let default = 1
    let name = "dedup"
    let doc = "Sets the backtrack depth in the list of insecurity \
               formulas when searching for duplicates.\n" ^
               "\t\t- dd=0: Disable backtrack\n" ^
               "\t\t- dd=n: Backtrack up to depth n\n" ^
               "\t\t- dd=-1: Backtrack until the end of the list"
  end
  )

module Untainting = Builder.True(
  struct
    let name = "untainting"
    let doc = "If enabled, the store, select and conditional \
               operations that are relational are simplified to a \
               simple expression."
  end
  )

module LowDecl = Builder.False(
  struct
    let name = "low-decl"
    let doc = "If enabled, explicitely declares variables at low \
               memory addresses."
  end
  )

module Canonical = Builder.True(
  struct
    let name = "canonical"
    let doc = "Enables canonical expressions in the store instead of \
               intermediate variables"
  end
  )

type store_type =
  | Sse
  | SelfComposed
  | Relational

module SymbolicStore = 
  Builder.Variant_choice_assoc(struct
      type t = store_type
      let name = "store-type"
      let doc = "Select the type of the symbolic store"
      let default = Relational
      let assoc_map = [
        "sse", Sse;
        "sc", SelfComposed;
        "sha", Relational;      (* Backward compatibility *)
        "rel", Relational]
    end)

type memory_type =
  | MemStd
  | MemList
  | MemMap

module MemoryType = 
  Builder.Variant_choice_assoc(struct
      type t = memory_type
      let name = "memory-type"
      let doc = "Select the type of the memory"
      let default = MemMap
      let assoc_map = [
        "std", MemStd;
        "row-list", MemList;
        "row-map", MemMap]
    end)

type leak_info =
  | HaltLeak                    (* Halts at first leak *)
  | InstrLeak                   (* Report leaky instructions
                                   (instructions are reported only
                                   once) *)
  | UniqueLeaks                 (* Repors unique expressions that are
                                   leaked along a path *)
(* | Quantified                 (\* Quantifies the amount of bytes/bis
   *                                 leaked per Program/Path/Instruction \
   *                                 Not implemented yet *\) *)
    
module LeakInfo =
  Builder.Variant_choice_assoc(struct
      type t = leak_info
      let name = "leak-info"
      let doc = "Select the information that is reported about leakage.\n" ^
                "\t\t- halt: halts at first leak\n" ^
                "\t\t- instr: report leaky instructions (instructions \
                 are reported only once)\n" ^
                "\t\t- unique-leak: repors unique expressions that are \
                 leaked along a path"
      let default = InstrLeak
      let assoc_map = [
        "halt", HaltLeak;
        "instr", InstrLeak;
        "unique-leak", UniqueLeaks]
    end)


(* Spectre *)

type spectre_pht = NoPHT
                 | Explicit        (* 4-way forking: 2 transient / 2 regular *)
                 | ExplicitSmarter (* 4-way forking with check_sat before solve (reduce queries) *)
                 | Haunted         (* Transient execution shadowing regular execution *)

module SpectrePht = 
  Builder.Variant_choice_assoc(struct
    type t = spectre_pht
    let name = "spectre-pht"
    let doc = "Select how to check for Spectre-PHT vulnerabilities.\n" ^
                "\t\t- none: do not check for spectre-pht\n" ^
                "\t\t- explicit: 4-way forking: 2 transient / 2 regular\n" ^
                "\t\t- explicit-smart: 4-way forking with check_sat \
                 before solve (reduce queries)\n" ^
                "\t\t- haunted: Transient execution shadowing regular execution\n"
      let assoc_map = [
        "none", NoPHT;
        "explicit", Explicit;
        "explicit-smart", ExplicitSmarter;
        "haunted", Haunted;]
    let default = NoPHT
  end
  )

type spectre_dynamic_pht =
  | Static          (* Speculation depth fixed to speculative window size *)
  | Hybrid of int   (* Speculation depth fixed to some constant when
                       conditonal does not depend from memory *)
  | FullyDynamic    (* Retire conditional when all corresponding
                       memory accesses are resolved *)

module DynamicPht = 
  Builder.Variant_choice_assoc(struct
    type t = spectre_dynamic_pht
    let name = "spectre-dyn-pht"
    let doc = "Select how to check for Spectre-PHT vulnerabilities.\n" ^
              "\t\t- none: Speculation depth fixed to speculative window size \n" ^
              "\t\t- hybrid0: Speculation depth fixed to 0 when \
                       conditonal does not depend from memory\n" ^
              "\t\t- hybrid20: Speculation depth fixed to 20 when \
                       conditonal does not depend from memory\n" ^
              "\t\t- full: Retire conditional when all corresponding \
                       memory accesses are resolved\n"
      let assoc_map = [
        "none", Static;
        "hybrid0", Hybrid 0;
        "hybrid20", Hybrid 20;
        "full", FullyDynamic;]
    let default = Static
  end
  )


module SpeculativeWindow = Builder.Integer(
  struct
    let default = 200
    let name = "speculative-window"
    let doc = "Sets the size of the speculative window (maximum speculative depth)"
  end
  )

type spectre_stl = NoSTL
                 | Explicit        (* Forks at each load/store interleaving *)
                 | HauntedIte      (* ite encoding of load expressions *)
module SpectreStl = 
  Builder.Variant_choice_assoc(struct
    type t = spectre_stl
    let name = "spectre-stl"
    let doc = "Select how to check for Spectre STL vulnerabilities.\n" ^
                "\t\t- none: do not check for Spectre STL\n" ^
                "\t\t- explicit: Forks at each load/store interleaving\n" ^
                "\t\t- haunted: Transient execution shadowing regular execution\n"
      let assoc_map = [
        "none", NoSTL;
        "explicit", Explicit;
        "haunted-ite", HauntedIte;]
    let default = NoSTL
  end
  )

module StoreBuffer = Builder.Integer(
  struct
    let default = 20
    let name = "store-buffer"
    let doc = "Sets the size of the store-buffer"
  end
  )

module SpectreMOB = Builder.False(
  struct
    let name = "spectre-mob"
    let doc = "If set, the branching predictor can also predict \
               store/load alias."
  end
  )
