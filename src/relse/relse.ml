open Relse_options

(** Enumerate jumps targets *)
let get_entry_point () =
  match Kernel_functions.get_ep () with
  | Some v -> v
  | None ->
    Logger.warning "No entrypoint: starting from main.";
    Relse_utils.get_main_symbol ()
    |> Loader_utils.address_of_symbol
    |> Virtual_address.create


module Insecurity_state = Relse_insecurity.Insecurity_State
module Path_state = Relse_path.Path_state
module Sym_state = Relse_symbolic.State

module State = struct
  type t = {
    path : Path_state.t;
    insecurity : Insecurity_state.t;
  }

  let create ~path ~insecurity = { path; insecurity; }

  let path state = state.path
  let insecurity state = state.insecurity

  let set_path state path = { state with path }

  (* let set_insecurity state insecurity = { state with insecurity } *)

  let on_path f state =
    { state with path = (f state.path) }

  (* let on_insecurity f state =
   *   { state with insecurity = (f state.insecurity) } *)

  let update_state f state =
    let (path, insecurity) = f state.path state.insecurity
    in { path; insecurity }
end

module State_stack = Fstack.Make(State)

module Env = struct

  type t = {
    (* The stack of states to explore *)
    worklist : State_stack.t;

    (* Targets of dynamic jumps *)
    djump_targets : Bitvector.t Virtual_address.Htbl.t;
    
    (* Transient states waiting for dynamic jumps to be computed *)
    sleeping : State.t Virtual_address.Htbl.t;

    (* The current state *)
    current_state : State.t option;

    (* The stub context which contains all the stub informations *)
    stub_ctx: Relse_stubs.t;  (* TODO also contains info relative to
                                 a specific path *)
  }

  let empty = 
    let worklist = State_stack.empty in
    let djump_targets = Virtual_address.Htbl.create 100 in
    let sleeping = Virtual_address.Htbl.create 100 in
    let initial_state = None in
    let stub_ctx = Relse_stubs.empty in      
    { worklist; djump_targets; sleeping; current_state=initial_state; stub_ctx; }

  (* Current environment *)
  let env = ref empty

  let stub_ctx () = !env.stub_ctx

  (* Initialize the environment starting at [entrypoint] *)
  let initialize_env ~initialize_fun ~entrypoint =
    let decode vaddress = Disasm_core.decode vaddress |> fst in
    let initial_instruction = decode entrypoint in
    let symbolic_state = Sym_state.create () in
    let initial_ps = Path_state.create symbolic_state initial_instruction
                     |> initialize_fun in
    let initial_is = Insecurity_state.create () in
    let initial_state = State.create ~path:initial_ps ~insecurity:initial_is in
    let worklist = State_stack.singleton initial_state in
    let stub_ctx = Relse_stubs.init () in
    env := { empty with worklist; stub_ctx }

  (* Chose a new path from the environment *)
  let next_state () =
    let worklist, current_state =
      match State_stack.pop !env.worklist with
      | current_state, worklist ->
        Logger.debug ~level:3 "[Exploration] Choose new path from environment :@ %a"
          Path_state.pp_loc (State.path current_state);
        worklist, Some current_state
      | exception Not_found when Virtual_address.Htbl.length !env.sleeping = 0 ->       
        !env.worklist, None
      | exception Not_found ->
        failwith "Worklist is empty but some states are still sleeping."
    in
    env := {!env with worklist; current_state }; current_state

  let push_state st =
    let worklist = State_stack.push st !env.worklist in
    env := { !env with worklist }

  let set_current_state st =
    env := { !env with current_state = Some st }
  
  let current_state () =
    match !env.current_state with
    | Some current_state -> current_state
    | None -> failwith "No current state in the environment"

  let put_state_to_sleep st =
    Virtual_address.Htbl.add !env.sleeping
      (Path_state.virtual_address @@ State.path st) st

  let wake_up_states address =
    List.iter push_state (Virtual_address.Htbl.find_all !env.sleeping address);
    let rec remove_all () =
      Virtual_address.Htbl.remove !env.sleeping address;
      if Virtual_address.Htbl.mem !env.sleeping address
      then remove_all ()
    in remove_all ()

  let get_djump_targets vaddr =
    Virtual_address.Htbl.find_all !env.djump_targets vaddr


  let add_djump_target vaddr bv =
    Virtual_address.Htbl.add !env.djump_targets vaddr bv
  
end

module Terminator = struct
  type t = {
    end_path   : Path_state.t -> bool;
    end_search : Path_state.t -> bool;
  }

  let create ~end_path ~end_search () =
    { end_path; end_search }

  let dfs ~goals ~avoids =
    let in_set ps = Virtual_address.Set.mem (Path_state.virtual_address ps) in
    let end_path ps =
      (* Avoid address *)
      if in_set ps avoids
      then (Logger.debug ~level:2 "[Exploration] End of path : to be avoided"; true)

      (* Max depth reached *)
      else if Sse_options.MaxDepth.get () > 0 &&
              Path_state.depth ps >= Sse_options.MaxDepth.get ()
      then
        begin
          Relse_stats.(set_status Status.Max_Depth);
          Logger.warning "[Exploration] Max depth exceeded (%d)"
          @@ Path_state.depth ps;
          true
        end
      else false
    and end_search ps = in_set ps goals in
    create ~end_search ~end_path ()
end

(** End the RelSE *)
let kill_relse msg =
  Logger.result msg;
  Relse_stats.print_stats ();
  exit (Relse_stats.get_exit_code ())

(** Terminates the current path *)
let kill_path () =
  let ps = Env.current_state () |> State.path in
  Relse_stats.add_path ();
  (match Path_state.speculative_mode ps with
   | Relse_path.Spectre.TransientMode -> Relse_stats.add_transient_path ()
   | Relse_path.Spectre.RegularMode -> ()
   | Relse_path.Spectre.HauntedMode -> Relse_stats.add_transient_path ());
  let path_number = Relse_stats.get_nb_paths () in
  Logger.debug ~level:4 "[Exploration] Path %d explored." path_number;
  (* Check if the max number of paths has been reached *)
  if Relse_options.MaxPaths.get () > 0 &&
     Relse_stats.get_nb_paths () >= Relse_options.MaxPaths.get ()
  then (Relse_stats.(set_status Status.Max_Paths);
        kill_relse "Maximum number of paths reached.")

(** Terminates the current path and the RelSE *)
let kill_path_and_relse msg =
  kill_path ();
  kill_relse msg

(* Check the remaining insecurity queries *)
let check_and_kill_path state =
  (* Check remaining insecurity queries *)
  ignore (Insecurity_state.check_insecurity
            Relse_insecurity.Path
            (State.path state)
            (State.insecurity state));
  kill_path ()


let get_avoid_address () =
  let addresses = Sse_options.AvoidAddresses.get () in

  let add_function_end func_name addresses =
    let img = Kernel_functions.get_img () in
    let _, end_fun = Loader_utils.symbol_by_name ~name:func_name img 
                     |> Utils.unsafe_get_opt
                     |> Loader_utils.symbol_interval in
    let end_fun = (Virtual_address.pred end_fun) in
    Logger.debug ~level:2 "[Initialization] Adding end of %s at %a to avoids."
      func_name Virtual_address.pp end_fun;
    Basic_types.Int.Set.add (Virtual_address.to_int end_fun) addresses
  in

  (* If entrypoint is a function, add ret from function as avoid address *)
  let addresses =
    match Kernel_options.Entry_point.get_opt () with
    | None -> add_function_end "main" addresses (* Means we start from main *)
    | Some s ->
      match Loader_utils.Binary_loc.of_string s with
      | Loader_utils.Binary_loc.Name func_name ->
        add_function_end func_name addresses
      | _ -> addresses
  in
  let pp_address addr =
    Logger.debug ~level:2 "[Initialization] Avoids %a"
      Virtual_address.pp (Virtual_address.create addr)
  in
  Basic_types.Int.Set.iter pp_address addresses;
  addresses


(* Initialize the registers to comply with the ABI *)
let initialize_registers ps =
  (* IOPL = 0 *)
  Logger.debug ~level:5 "[initialisation] Setting IOPL to 0";
  let rval = Rel_expr.mk_simple (Formula.mk_bv_zeros 2) in
  let lval = Dba.LValue.var (X86Util.flag_to_string X86Types.IOPL)
      ~bitsize:(Size.Bit.create 2) ~tag:(Dba.VarTag.flag Dba.Flag.unspecified) in
  let ps = Relse_smt.Translate.assignment ~init:true lval rval ps in
  (* DF = 0 *)
  Logger.debug ~level:5 "[initialisation] Setting DF to 0";
  let rval = Rel_expr.mk_simple (Formula.mk_bv_zero) in
  let lval = Dba.LValue.var (X86Util.flag_to_string X86Types.DF)
      ~bitsize:(Size.Bit.bits1) ~tag:(Dba.VarTag.flag Dba.Flag.unspecified) in
  Relse_smt.Translate.assignment ~init:true lval rval ps

(** Initialize symbolic execution with temporary values for high variables *)
let initialize_highs_in_stack ps =
  let init_high_x86 offset ps =
    (* @[esp-{offset}] := <s | s'> *)
    let bv_offset = Relse_utils.int_to_bitvector (offset + 4) in
    let name = "@esp-" ^ (string_of_int offset) in
    let msg = "Setting high byte: " ^ name  in
    Logger.debug ~level:5 "[initialisation] %s" msg;
    let ps = Path_state.maybe_add_comment ps msg in
    (* Declare symbolic high byte *)
    let sort = Formula.bv_sort (Natural.to_int Basic_types.Constants.bytesize) in
    let ps = Path_state.declare_high name sort ps in
    (* Store it in the memory *)
    let tag = Dba.VarTag.register in
    let size = Size.Bit.to_int (Relse_utils.word_size_bits ()) in
    let esp_reg = Dba.Expr.var ~tag "esp" size in
    let addr = Dba.Expr.(sub esp_reg (constant bv_offset)) in
    let lval = Dba.LValue.store (Size.Byte.create 1) (Kernel_options.Machine.endianness ()) addr in
    let rval = Dba.Expr.temporary ~size:(Natural.to_int Basic_types.Constants.bytesize) name in
    let rval = Relse_smt.Translate.expr_noset ps rval in 
    Relse_smt.Translate.assignment ~init:true lval rval ps
  in
  let default _ _ =
    failwith "Init high not implemented for this architecture"
  in
  let f = match Kernel_options.Machine.get () with
    | Machine.X86 _ -> init_high_x86
    | _ -> default
  in
  Basic_types.Int.Set.fold f (Relse_options.HighBytes.get ()) ps

let initialize_high_symbols ps =
  let init_high symbol_name ps =
    let img = (Kernel_functions.get_img ()) in
    let symbol = match Loader_utils.symbol_by_name ~name:symbol_name img with
      | Some symbol -> symbol
      | None -> failwith ("No symbol named " ^ symbol_name) in
    let size = Loader_utils.size_of_symbol symbol in
    let base_addr = Loader_utils.address_of_symbol symbol in
    let base_vaddr = Virtual_address.of_int64 (Int64.of_int base_addr) in
    Logger.debug ~level:3 "[Initialisation][high_symbol] %@%s[%d] at addr %a"
      symbol_name size Virtual_address.pp base_vaddr;
    (* TODO mutualize this function with delace_symbolic_size in relse_stubs.ml *)
    let rec loop offset ps =
      if offset = size then ps
      else
        let var_name = Format.asprintf "h_%s_%d" symbol_name offset in
        Logger.debug ~level:5 "[Initialisation][high_byte] %@%s[%d] := %s"
          symbol_name offset var_name;
        (* Declare symbolic high byte *)
        let sort = Formula.bv_sort (Natural.to_int Basic_types.Constants.bytesize) in
        let ps = Path_state.declare_high var_name sort ps in
        (* Store it in the memory *)
        let addr = offset + Virtual_address.to_int base_vaddr
                   |> Relse_utils.dba_constant_from_int in
        Logger.debug ~level:5 "[Initialisation] %@[%a] := %s"
          Dba_printer.Ascii.pp_bl_term addr var_name;
        let lval = Dba.LValue.store (Size.Byte.create 1) (Kernel_options.Machine.endianness ()) addr in
        let size = (Natural.to_int Basic_types.Constants.bytesize) in
        let rval = Dba.Expr.temporary ~size var_name in
        let rval = Relse_smt.Translate.expr_noset ps rval in 
        let ps = Relse_smt.Translate.assignment ~init:true lval rval ps
        in loop (offset + 1) ps
    in
    loop 0 ps
  in
  Basic_types.String.Set.fold init_high (Relse_options.HighSymbols.get ()) ps

(** Initialize the initial memory of path state [ps] from [filename]
    (default memory.txt) *)
let init_from_file ~filename ps =
  if not (Sys.file_exists filename) then begin
    Logger.warning "Cannot find sse configuration file %s" filename;
    ps
  end
  else
    let initials =
      Logger.debug "Reading initialization from %s" filename;
      let parser = Parser.initialization
      and lexer = Lexer.token in
      Parse_utils.read_file ~parser ~lexer ~filename
    in
    let f ps init =
      let open Parse_helpers.Initialization in
      match init.operation with
      | Mem_load (addr, size) ->
        Path_state.with_init_mem_at ps ~addr ~size
      | Universal lval ->
        begin
          match Dba_types.LValue.name_of lval with
          | Some name ->
            let size = Dba.LValue.size_of lval in
            let sort = Formula.BvSort size in
            Path_state.declare_low name sort ps
          | None -> ps
        end
      | Assignment (lval, rval, _) ->
        match rval with
        | Singleton rval ->
          let rval = Relse_smt.Translate.expr_noset ps rval in 
          Relse_smt.Translate.assignment ~init:true lval rval ps
        | _ ->
          failwith "Not implemented yet"
    in List.fold_left f ps initials

let initialize_state ~filename ps =
  init_from_file ~filename ps       
  |> initialize_registers
  |> initialize_highs_in_stack
  |> initialize_high_symbols

let check_sat state =
  let ps = State.path state in
  match Relse_smt.Solver.check_sat_pc ~retire_stl:false ps with
  | Formula.SAT, ps ->
    let state = State.set_path state (Path_state.mark_sat ps) in
    Some state
  | _ -> None

(** Pick the next satisfiable branch in the worklist *)
let rec choose_next_state () =
  match Env.next_state () with
  | Some state ->
    (match check_sat state with
     | Some state -> state
     | None -> Relse_stats.discard_path (); choose_next_state ())
  | None -> (* No more paths *)
    kill_relse "[Exploration] End of the RelSE"


module Eval = struct

  let static_jump ~jump_target ps =
    match jump_target with
    | Dba.JInner idx -> Path_state.set_block_index idx ps
    | Dba.JOuter addr ->
      let vaddr = Dba_types.Caddress.to_virtual_address addr in
      Logger.debug ~level:5 "Jumping to new address %a"
        Virtual_address.pp vaddr;
      Path_state.goto_vaddr vaddr ps

  let add_call ~ret_addr ps =
    let ret_addr = Dba_types.Caddress.to_virtual_address ret_addr in
    Logger.debug ~level:5 "[Exploration] Call with return address %a"
      Virtual_address.pp ret_addr;
    Path_state.push_ret ret_addr ps

  let get_retire_depth state r_cond =
    let current_depth = Path_state.depth (State.path state) in
    let max_spec_depth = Relse_options.SpeculativeWindow.get () in
    match Relse_options.DynamicPht.get () with
    | Relse_options.Static -> max_spec_depth + current_depth
    | Relse_options.Hybrid _  when
        (* Expression depends on memory *)
        Relse_utils.Spectre.unresolved_mem_access r_cond current_depth ->
      Logger.debug ~level:2 "[Spectre][Dynamic][fork][unresolved] %a at depth %d"
        (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_cond current_depth;
      Relse_stats.add_unresolved_fork ();
      max_spec_depth + current_depth
    | Relse_options.Hybrid n ->
      (* Expression does not depend on memory *)
      Logger.debug ~level:2 "[Spectre][Dynamic][fork][resolved] %a at depth %d"
        (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_cond current_depth;
      Relse_stats.add_resolved_fork ();
      n + current_depth
    | Relse_options.FullyDynamic ->
      (* Retire conditional when memory access is retired *)
      (* Expression does not depend on memory *)
      let retire_depth = match Rel_expr.get_date r_cond with
        | Some date -> date + max_spec_depth
        | None -> current_depth in
      Logger.debug ~level:2 "[Spectre][Dynamic][fork][Full] set retire \
                             depth to %d for %a at depth %d"
        retire_depth (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_cond current_depth;
      if retire_depth > current_depth
      then Relse_stats.add_unresolved_fork ()
      else Relse_stats.add_resolved_fork ();
      retire_depth

  let fork_path ~is_conditional ~r_cond ~true_target ~false_target state =
    (* Updates cfg with new fork *)
    (* Path_state.add_conditional_to_cfg ~true_target ~false_target (State.path state); *)
    Relse_stats.add_fork ();
      
    (* Prepare conditional expressions *)
    let mk_true_cond expr = Formula.(mk_bv_equal expr mk_bv_one) in
    let true_cond = Rel_expr.apply mk_true_cond r_cond in
    let mk_false_cond expr = Formula.mk_bl_not (mk_true_cond expr) in
    let false_cond = Rel_expr.apply mk_false_cond r_cond in
    
    let add_cond cond state =
      State.on_path (Path_state.update_pc_cond cond) state
    in
    let goto_true state =
      State.on_path (static_jump ~jump_target:true_target) state
    in
    let goto_false state =
      State.on_path (Path_state.set_block_index false_target) state
    in

    let retire_depth = get_retire_depth state r_cond in
    (* Expand current path with assert condition and go to true_target *)
    let true_state = state |> add_cond true_cond |> goto_true in
    (* Push path with negation of the condition and go to false_target *)
    let false_state = state |> add_cond false_cond |> goto_false in

    match is_conditional, Relse_options.SpectrePht.get () with
    | false, _ | _, NoPHT ->
      (* Warning: pushing both states on the queue without
         checking if they are satisfiable might lead to
         performance issues *)
      Env.push_state true_state; Env.push_state false_state; choose_next_state ()

    | true, Explicit ->
      Logger.debug ~level:2 "[Spectre] Speculative execution of %a"
        (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_cond;

      (* Transient execution of true path when conditional is false *)
      let spec_true_state =
        state |> add_cond false_cond |> goto_true
        |> State.on_path (Path_state.start_explicit_transient_path ~retire_depth) in

      (* Transient execution of false path when conditional is true *)
      let spec_false_state =
        state |> add_cond true_cond |> goto_false
        |> State.on_path (Path_state.start_explicit_transient_path ~retire_depth) in

      (* Warning: pushing both states on the queue without
         checking if they are satisfiable might lead to
         performance issues *)
      (* Env.push_state true_state; Env.push_state spec_true_state;
       * Env.push_state false_state; Env.push_state spec_false_state; *)
      Env.push_state true_state; Env.push_state false_state; 
      Env.push_state spec_true_state; Env.push_state spec_false_state;
      choose_next_state ()

    | true, ExplicitSmarter ->
      Logger.debug ~level:2 "[Spectre] Speculative execution of %a"
        (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_cond;
      
      (* Add conditional = false to ps *)
      let false_cond_state = state |> add_cond false_cond in
      let false_state, spec_true_state =
        match check_sat false_cond_state with
        | Some state ->
          let false_state = state |> goto_false in
          let spec_true_state =
            state |> goto_true
            |> State.on_path (Path_state.start_explicit_transient_path ~retire_depth) in
          Some false_state, Some spec_true_state
        | None -> None, None
      in

      (* Add conditional = true to ps *)
      let true_cond_state = state |> add_cond true_cond in
      let true_state, spec_false_state =
        match check_sat true_cond_state with
        | Some state ->
          let true_state = state |> goto_true in
          let spec_false_state =
            state |> goto_false 
            |> State.on_path (Path_state.start_explicit_transient_path ~retire_depth) in
          Some true_state, Some spec_false_state
        | None -> None, None
      in

      (* Makes sure that states are pushd in the same order as in the
         Naive Explicit case *)
      let push_option = function
        | Some state -> Env.push_state state
        | None -> Relse_stats.discard_path ()
      in
      (* push_option true_state; push_option spec_true_state;
       * push_option false_state; push_option spec_false_state; *)
      push_option true_state; push_option false_state;
      push_option spec_true_state; push_option spec_false_state;
      choose_next_state ()

    | true, Haunted ->
      Logger.debug ~level:2 "[Spectre] Speculative execution of %a"
        (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_cond;
      
      (* Speculative execution of true path *)
      let spec_true_state =
        state |> goto_true |> State.on_path (Path_state.start_haunted_path
                                               ~retire_depth
                                               ~regular_cond:true_cond
                                               ~transient_cond:false_cond) in

      (* Transient execution of false path when conditional is true *)
      let spec_false_state =
        state |> goto_false |> State.on_path (Path_state.start_haunted_path
                                                ~retire_depth
                                                ~regular_cond:false_cond
                                                ~transient_cond:true_cond) in
      (* Prioritize transient paths *)
      if Formula_utils.is_bl_true @@ Rel_expr.apply_lr Formula.mk_bl_and false_cond then
        (Env.push_state spec_false_state; Env.push_state spec_true_state)
      else
        (Env.push_state spec_true_state; Env.push_state spec_false_state);
      choose_next_state ()


  let ite ~condition ~true_target ~false_target state =
    let ps = State.path state in

    Logger.debug ~level:2 "[Exploration][Ite] Condition %a"
      Dba_types.Expr.pp condition;

    (* Check if the instruction is a conditional jump *)
    if Relse_utils.is_conditional_jump (Path_state.get_instruction ps)
    then
      begin
        if Dba.Expr.is_constant condition then
          failwith "Conditional jump with constant condition.";

        Relse_stats.add_conditional ();
        let r_cond = Relse_smt.Translate.expr_noset ps condition in
        let state = 
          (* Add condition to insecurity queries *)
          State.update_state
            (fun ps is ->
               let is = Insecurity_state.add_cf_check r_cond is in
               let ps, is = Insecurity_state.check_insecurity Relse_insecurity.Block ps is in
               (* Untaint the condition since both programs follow the same path *)
               let ps = Path_state.untaint ps [r_cond] in
               (* TODO already done in insecurity checker ? *)
               ps, is)
            state
        in
        (* TODO try if adding the untainted version of r_cond improves perfs *)
        fork_path ~is_conditional:true ~r_cond ~true_target ~false_target state
      end
    else (* Instruction is not a conditional jump *)
    if not @@ Dba.Expr.is_constant condition then
      begin
        (* Logger.warning
         * "@[<hov>Fork on Ite expression %a at address %a (not a conditional)@]"
         * Dba_printer.Ascii.pp_instruction (Path_state.get_dba_instruction ps)
         * Path_state.pp_loc ps; *)
        let r_cond = Relse_smt.Translate.expr_noset ps condition in
        fork_path ~is_conditional:false ~r_cond ~true_target ~false_target state
      end
    else
      (* Set next instruction *)
      let state =
        if Dba.Expr.(is_equal condition _false)
        then
          State.on_path (static_jump ~jump_target:true_target) state
        else
          begin if Dba.Expr.(is_equal condition _true)
            then State.on_path (Path_state.set_block_index false_target) state
            else failwith "Conditional neither true or false"
          end
      in
      Env.push_state state; choose_next_state ()


  let update_state_with_transient_values update_state rvalue =
    match Relse_options.SpectreStl.get () with
    | Relse_options.NoSTL ->
      if Relse_term.TransientSet.has_transient rvalue
      then assert false
      else
        let rvalue = Relse_term.TransientSet.regular rvalue in
        update_state rvalue

    | Relse_options.Explicit ->
      (* Push regular execution to worklist *)
      let regular_rvalue = Relse_term.TransientSet.regular rvalue in
      let regular_state = update_state regular_rvalue in
      Env.push_state regular_state;
      (* For each transient term, add new path in the worklist *)
      let tt_set = Relse_term.TransientSet.transient rvalue in
      Relse_term.TTHashSet.iter (fun tt ->
          Logger.debug ~level:2 "[SpectreSTL] Starting path for \
                                 transient load %a"
            Relse_term.Transient.pp tt;
          Relse_stats.add_transient_load ();
          let rvalue = Relse_term.Transient.value tt in
          let retire_depth = Relse_term.Transient.retire_depth tt
                             |> Utils.unsafe_get_opt in
          let new_state = update_state rvalue |>
                          State.on_path (Path_state.start_transient_load ~retire_depth) in
          Env.push_state new_state)
        tt_set;
      choose_next_state ()

    | Relse_options.HauntedIte ->
      let rvalue = Relse_term.TransientSet.to_ite rvalue in
      update_state rvalue

  
  (* Should only be used in non transient execution *)
  let compute_djump_targets r_expr state =
    (* Enumerate targets *)
    let ps = State.path state in
    let n = Sse_options.JumpEnumDepth.get () in
    let target = Rel_expr.left r_expr in
    let targets, ps = match Formula_utils.is_bv_cst target with
      | Some bv -> (* If the target expression is concrete, goto jump target *)
        [bv], ps
      | None -> (* If the target expression is symbolic, enumerate jump target *)
        (* Current behavior: enumerate indirect jump values in regular & transient execution *)
        let pc = Relse_utils.get_formula @@ Path_state.path_constraint ps in
        let targets, ps = Relse_smt.Solver.enumerate_values ~retire_stl:true n target pc ps in
        if List.length targets >= n then
          Relse_stats.enum_limit_reached ();
        targets, ps in
    (* Remove invalid targets *)
    let img = Kernel_functions.get_img () in
    let is_valid bv =
      Logger.debug ~level:4 "[Exploration] Dynamic jump@ %a@ could lead to %a"
        Path_state.pp_loc ps
        Bitvector.pp_hex bv;
      let address = Bitvector.to_int bv in
      let section = Loader_utils.find_section_by_address ~address img in
      match section with
      | Some s when
          Loader.Section.has_flag Loader_types.Read s &&
          Loader.Section.has_flag Loader_types.Exec s ->
        (* Add valid jump target to the environment *)
        Env.add_djump_target (Path_state.virtual_address ps) bv;
        true
      | Some _ | None ->
        Logger.warning "@[<hov>Dynamic jump@ %a@ could have led to \
                        invalid address %a;@ skipping@]"
          Path_state.pp_loc ps Bitvector.pp_hex bv;
        false
    in
    List.filter is_valid targets

  
  let djump_to_targets targets r_expr state =
    let ps = State.path state in 
    let goto_target ps bv =
      (* Add the condition to the path constraint *)
      let mk_cond expr = Formula.(mk_bv_equal (mk_bv_cst bv) expr) in
      let condition = Rel_expr.apply mk_cond r_expr in
      let ps = (Path_state.update_pc_dynamic ~checked:true condition) ps
               |> Path_state.goto_vaddr (Virtual_address.of_bitvector bv) in
      Env.push_state (State.set_path state ps)
    in
    List.iter (goto_target ps) targets

  let do_return r_expr state =
    State.on_path (fun ps ->
        let addr, ps = Path_state.pop_ret ps in
        Logger.debug ~level:5 "[Exploration][ShadowStack] Return to address %a"
          Virtual_address.pp addr;
        (* If the jump expression is constant, check that it matches
           addr. *)
        (if not @@ Relse_term.TransientSet.has_transient r_expr then
           let r_expr = Relse_term.TransientSet.regular r_expr in
           if not @@ Rel_expr.is_relational r_expr then
             let expr = Rel_expr.value r_expr in
             match Formula_utils.is_bv_cst expr with
             | Some bv when not @@ Virtual_address.equal addr
                   (Virtual_address.of_bitvector bv) ->
               let error_msg = Format.asprintf
                   "Return address from SE (%a) does not \
                    match return address from Shadow \
                    Stack (%a)"
                   Virtual_address.pp (Virtual_address.of_bitvector bv)
                   Virtual_address.pp addr in
               failwith error_msg
             | _ -> ());
        Path_state.goto_vaddr addr ps)
      state

  let dynamic_jump ~jump_expr tag state =
    let r_expr = Relse_smt.Translate.expr (State.path state) jump_expr in
    (* Add insecurity check *)
    let state = State.update_state
        (fun ps is ->
           let is = Relse_term.TransientSet.fold Insecurity_state.add_cf_check r_expr is in
           let ps, is = Insecurity_state.check_insecurity Relse_insecurity.Block ps is in
           (* TODO (in insecurity module ?)
              Untaint the condition since both programs follow the same path *)
           (* let ps = Path_state.untaint ps [r_expr] in *)
           ps, is)
        state in

    (* Do return *)
    if Dhunk.is_return @@ Instruction.hunk @@ Path_state.get_instruction @@
      State.path state then do_return r_expr state
    else
      (* Do dynamic jump *)
      let state = State.on_path (fun ps -> match tag with
          | Some (Dba.Call ret_addr) -> add_call ~ret_addr ps
          | _ -> ps)
          state in
      let ps = State.path state in
      if Relse_term.TransientSet.has_transient r_expr then
        Relse_stats.add_transient_dynamic_jump ();
      Logger.debug ~level:9
        "@[<hov>Return@ %a@ discarding transient values of@ %a@]"
        Path_state.pp_loc ps Relse_term.TransientSet.pp r_expr;
      let r_expr = Relse_term.TransientSet.regular r_expr in
      match Path_state.speculative_mode ps with
      | Relse_path.Spectre.TransientMode when SpectreStl.get () <> NoSTL ->
        (match Env.get_djump_targets (Path_state.virtual_address ps) with
         | [] -> (* Jump targets not computed yet *)
           (* Put the current path state in the sleeping list *)
           Logger.debug ~level:3 "[Spectre] Putting current speculative state to sleep";
           Env.put_state_to_sleep state;
           choose_next_state ()
         | targets ->
           Logger.debug ~level:3 "[Spectre] Speculative state can continue on computed targets";
           (* Jump to the computed targets *)
           djump_to_targets targets r_expr state;
           choose_next_state ())
      | Relse_path.Spectre.HauntedMode
      | Relse_path.Spectre.TransientMode
      | Relse_path.Spectre.RegularMode ->
        (* Just compute new targets and jump on them *)
        let targets = compute_djump_targets r_expr state in
        djump_to_targets targets r_expr state;
        Env.wake_up_states @@ Path_state.virtual_address ps;
        choose_next_state ()

  let skip instruction idx =
    Logger.debug ~level:3 "[Exploration] Skipping %a" Dba_printer.Ascii.pp_instruction instruction;
    Path_state.set_block_index idx


  (* If comment is activated, this will add, for every formula entry, a
     comment about where it comes from.
     This can be usefull to debug the path predicate translation.  *)
  let maybe_add_comment ps =
    let comment =
      Print_utils.string_from_pp
        (Formula_pp.pp_as_comment Path_state.pp_loc) ps
    in Path_state.maybe_add_comment ps comment

  (** Evaluation of a DBA instuction in the symbolic environment *)
  let eval state =
    let state = State.on_path maybe_add_comment state in
    let ps = State.path state in
    Logger.debug ~level:5 "@[Evaluating@ %a@]" Path_state.pp_loc ps;

    (* Check for stubs *)
    match Relse_stubs.check (Env.stub_ctx ()) ps with
    | Relse_stubs.Halt ->
      check_and_kill_path state; choose_next_state ()
    | Relse_stubs.Skip ps ->
      State.set_path state ps

    | Relse_stubs.Continue ps ->
      let state = State.set_path state ps in
      let dba_instr = Path_state.get_dba_instruction ps in

      (* Gather (and possibly check) the memory insecurity queries of
         the current instruction TODO -> move conditional checks in
         the insecurity module *)
      let state =
        let update_insecurity ps is =
          let is = Insecurity_state.add_memory_check ps dba_instr is in
          Insecurity_state.check_insecurity Relse_insecurity.Instr ps is
        in State.update_state update_insecurity state
      in

      (* evaluate the instruction
         TODO: harmonization of functions / style *)
      match dba_instr with
      | Dba.Instr.Assign (lvalue, rvalue, idx) ->
        let rvalue = Relse_smt.Translate.expr (State.path state) rvalue in
        let state = State.on_path (Path_state.set_block_index idx) state in

        let update_state rvalue =
          State.on_path (Relse_smt.Translate.assignment lvalue rvalue) state
        in
        update_state_with_transient_values update_state rvalue

      | Dba.Instr.SJump (Dba.JOuter addr, _) when
          (Virtual_address.to_int @@ Dba_types.Caddress.to_virtual_address addr) = 0 ->
        (* Jump to address 0 *)
        Logger.warning "[Exploration] Static jump to 0";
        check_and_kill_path state; choose_next_state ()

      | Dba.Instr.SJump (Dba.JOuter addr, Some (Dba.Call ret_addr)) ->
        (* Call instruction *)
        State.on_path (fun ps ->
            add_call ~ret_addr ps
            |> static_jump ~jump_target:(Dba.JOuter addr))
          state
  
      | Dba.Instr.SJump (jump_target, _) ->
        State.on_path (static_jump ~jump_target) state

      | Dba.Instr.If (condition, jump_target, local_target) ->
        ite ~condition ~true_target:jump_target ~false_target:local_target state

      | Dba.Instr.DJump (e, tag) ->
        (* Check if dynamic jump refers to gs *)
        let gs_register = Dba.Expr.var ~tag:Dba.VarTag.empty "gs_base" @@
          Size.Bit.to_int @@ Relse_utils.word_size_bits () in
        if Dba_utils.contains_dba_expr gs_register e
        then
          begin
            Logger.warning "@[<hov>Dynamic jump@ %a@ Unsupported syscall@]"
              Virtual_address.pp @@ Path_state.virtual_address ps;
            Relse_stats.(set_status Status.Unsupported_Syscall);
            check_and_kill_path state; choose_next_state ()
          end
        else dynamic_jump ~jump_expr:e tag state
    
      | Dba.Instr.Stop (Some Dba.KO)
      | Dba.Instr.Stop (Some Dba.OK) ->
        (* Discard current path, choose a new one *) 
        check_and_kill_path state; choose_next_state ()

      | Dba.Instr.Undef (_, idx) as instruction ->
        (* Instruction [lval := undef] -> Ignores the instruction *)
	      State.on_path (skip instruction idx) state

      | Dba.Instr.Assert (expr, idx) as dba_instruction ->
        let expr = Relse_smt.Translate.expr_noset ps expr |> Rel_expr.value in
        (match Formula_utils.is_bv_cst expr with
         | Some bv when Bitvector.is_one bv ->
           (* Assert is verified *)
	         State.on_path (skip dba_instruction idx) state
         | Some bv when Bitvector.is_zero bv ->
           (* Assert failed *)
           let vaddress = Path_state.location ps in
           let msg =
             Format.asprintf "assert %a failed at address %a"
               Dba_printer.Ascii.pp_instruction dba_instruction
               Virtual_address.pp (Dba_types.Caddress.to_virtual_address vaddress) 
           in Errors.not_yet_implemented msg
         | Some _ -> assert false
         | None ->               (* TODO: symbolic assert *)
           let vaddress = Path_state.location ps in
           let msg =
             Format.asprintf "symbolic assert %a at address %a"
               Dba_printer.Ascii.pp_instruction dba_instruction
               Virtual_address.pp (Dba_types.Caddress.to_virtual_address vaddress) 
           in Errors.not_yet_implemented msg)

      (* Fence *)
      | Dba.Instr.Serialize (_, idx) as dba_instruction ->
        begin match Path_state.fence ps with
        | Relse_path.Continue ps -> skip dba_instruction idx ps |> State.set_path state
        | Relse_path.Halt -> check_and_kill_path state; choose_next_state ()
        end

      | Dba.Instr.Stop _
      | Dba.Instr.Assume _
      | Dba.Instr.Nondet _
      | Dba.Instr.NondetAssume _
      | Dba.Instr.Malloc _
      | Dba.Instr.Free _
      | Dba.Instr.Print _ as dba_instruction ->
        let vaddress = Path_state.location ps in
        let msg =
          Format.asprintf "instruction %a at address %a"
            Dba_printer.Ascii.pp_instruction dba_instruction
            Virtual_address.pp (Dba_types.Caddress.to_virtual_address vaddress) 
        in Errors.not_yet_implemented msg
end


(** Explores all the symbolic paths *)
let loop_until ~p =
  let rec loop_aux state =
    let open Terminator in
    Env.set_current_state state;
    let ps = (State.path state) in
    (* Reached the goal (and the end of the path) *)  
    if p.end_search ps then
      begin
        let path_number = Relse_stats.get_nb_paths () in
        Path_state.pp_path ps path_number;
        check_and_kill_path state;
        kill_relse "Goal reached"
      end
      (* Reached the end of the path *)
    else if p.end_path ps then
      begin
        check_and_kill_path state;
        choose_next_state () |> loop_aux
      end
    else 
      (* Check if speculative depth has been reached *)
      match Path_state.maybe_retire ps with
      | Relse_path.Continue ps ->
        (* Evaluate the state *)
        begin
          match State.set_path state ps |> check_sat with
          | Some state -> state |> Eval.eval |> loop_aux
          | None ->          
            (* Path became unsat *)
            check_and_kill_path state;
            choose_next_state () |> loop_aux
        end
      | Relse_path.Halt ->
        check_and_kill_path state;
        choose_next_state () |> loop_aux
  in
  choose_next_state () |> loop_aux

(** Run the relational symbolic execution on the specified file *)
let do_relse ~filename =
  Logger.debug ~level:2 "[Initialization] Running RelSE with %s" filename;
  let entrypoint = get_entry_point () in
  Logger.debug ~level:2 "[Initialization] Starting from %a" Virtual_address.pp
    entrypoint;
  let ints_to_vaddresses iset =
    Basic_types.Int.Set.fold
      (fun i vset -> Virtual_address.(Set.add (create i) vset))
      iset
      Virtual_address.Set.empty
  in
  let p =
    Terminator.dfs
      ~goals:(Sse_options.GoalAddresses.get () |> ints_to_vaddresses)
      ~avoids:(get_avoid_address () |> ints_to_vaddresses) in
  Env.initialize_env
    ~initialize_fun:(fun st ->
        let filename = Sse_options.MemoryFile.get () in
        initialize_state ~filename st)
    ~entrypoint;
  loop_until ~p

let start () =
  let filename = Kernel_options.ExecFile.get () in
  do_relse ~filename

let timeout_handler signal =
  if signal == Sys.sigalrm then
    begin
      Relse_stats.(set_status Status.Timeout);
      kill_path_and_relse "[Exploration] Timeout of the RelSE"
    end
  else if signal == Sys.sigint then
    begin
      Relse_stats.(set_status Status.Interrupt);
      kill_path_and_relse "[Exploration] Interruption of the RelSE"
    end
  else if signal == Sys.sigterm then
    begin
      Relse_stats.(set_status Status.OOM);
      kill_path_and_relse "[Exploration] RelSE ran out of memory"
    end
  else
    begin
      Relse_stats.(set_status Status.Interrupt);
      kill_path_and_relse "[Exploration] RelSE was killed"
    end

(** Run the relational symbolic execution *)
let run () =
  Printf.printf "Running relse\n";
  if Relse_options.is_enabled () && Kernel_options.ExecFile.is_set () then
    begin
      (* Redirect interrupts to handler *)
      Sys.(set_signal sigalrm (Signal_handle timeout_handler));
      Sys.(set_signal sigterm (Signal_handle timeout_handler));
      Sys.(set_signal sigint (Signal_handle timeout_handler));
      (* Set gloabl timeout *)
      let set_timeout t = if t > 0 then Unix.alarm t |> ignore in 
      set_timeout @@ Relse_options.Timeout.get ();
      start ()
    end

let _ =
  Cli.Boot.enlist ~name:"RelSE" ~f:run
