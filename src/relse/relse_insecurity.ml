open Relse_options

type level = Path | Block | Instr
type secure = Secure | Insecure | Unknown

let dd () = Relse_options.Dedup.get ()

module Path_state = Relse_path.Path_state
module Solver = Relse_smt.Solver

module Insec = struct
  type t = {
    store : Rel_expr.rel_bv list;
    load : Rel_expr.rel_bv list;
    cond : Rel_expr.rel_bv list;
  }

  let empty = { store = []; load = []; cond = []; }

  let is_rel rel_bv =
    (* Check if the expression is relational *)
    if not (Rel_expr.(deduplicate_eq Formula.equal_bv_term rel_bv |> is_relational))
    then
      begin
        (* Spared check *)
        Relse_stats.add_spared_check ();
        Logger.debug ~level:3
          "[Insecurity][spared] %a is simple" 
          (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) rel_bv;
        false
      end
    else true

  let is_duplicate rel_bv current_list =
    (* Backtracks according to dd parameter to check if [rel_bv] is
       already in [current_list]. Record the number of checks spared /
       checks added. *)
    let equal_term t t' = Rel_expr.equals Formula.equal_bv_term t t' in
    let rec aux l fuel =
      if fuel = 0 then false
      else match l with
        | [] -> false
        | x :: xs ->
          if equal_term x rel_bv then
            begin
              Relse_stats.add_spared_check ();
              Logger.debug ~level:9
                "[Insecurity][add_check] Dedup hit: check %a already in the list"
                (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) rel_bv;
              true
            end
          else aux xs (fuel-1)
    in aux current_list (dd ())

  let maybe_add rel_bv current_list =
    if is_rel rel_bv && not (is_duplicate rel_bv current_list) then
      begin
        Relse_stats.add_done_check ();
        Logger.debug ~level:9
          "[Insecurity][add_check] Check %a added to the list"
          (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) rel_bv;
        rel_bv :: current_list
      end
    else current_list
      
  let add_store t rel_bv =
    { t with store = maybe_add rel_bv t.store }

  let add_load t rel_bv =
    { t with load = maybe_add rel_bv t.load }

  let add_cond t rel_bv =
    { t with cond = maybe_add rel_bv t.cond }

  let get_store t = t.store
  let get_load t = t.load
  let get_cond t = t.cond

end

(*----------------------------------
 |  Eval_dba to get memory accesses
 ----------------------------------*)
module Eval_dba = struct
  module Translate = Relse_smt.Translate
  
  let rec get_leaks_expr expr sym_state insec =
    let open Dba.Expr in
    match expr with
    | Var _ -> insec
    | Cst _ -> insec

    (* Leak index of the load *)
    | Load (_, _, idx) ->
      let r_idx = Translate.expr_noset sym_state idx in
      get_leaks_expr idx sym_state (Insec.add_load insec r_idx)

    | Binary (_, lop, rop) ->
      get_leaks_expr lop sym_state insec
      |> get_leaks_expr rop sym_state

    | Unary (_, e) ->
      get_leaks_expr e sym_state insec

    | Ite (c, then_e, else_e) ->
      get_leaks_expr c sym_state insec
      |> get_leaks_expr then_e sym_state
      |> get_leaks_expr else_e sym_state

  let get_leaks_lval lval sym_state insec =
    let open Dba.LValue in
    match lval with
    | Var _ -> insec
    | Restrict _ -> insec
    | Store (_, _, idx) ->
      let r_idx = Translate.expr_noset sym_state idx in
      (* Extend expressions to check with the index of the store *)
      get_leaks_expr idx sym_state (Insec.add_store insec r_idx)

  (** [get_memory_accesses dba_instr sym_state] Returns the list of memory
      accesses occuring when [dba_instr] is executed in [sym_state]*)
  let add_memory_accesses dba_instr sym_state insec =
    let open Dba.Instr in
    match dba_instr with
    | Assign (lvalue, rvalue, _) ->
      get_leaks_expr rvalue sym_state insec
      |> get_leaks_lval lvalue sym_state
    | SJump _ -> insec
    | If (condition, _, _) ->
      get_leaks_expr condition sym_state insec
    | DJump (e, _) ->
      get_leaks_expr e sym_state insec
    | _ -> insec
end

(*----------------------------------
 |  Insecurtiy Formula
 ----------------------------------*)
module Insecurity_formula :
sig
  type t
  val empty : t
  val add_checks : t -> (Insec.t  -> Insec.t) -> t
  val solve : Relse_stats.query_type -> Path_state.t -> t -> Path_state.t
end = struct
  
  type t = {
    (* The list of insecurity checks *)
    check_list: Insec.t;    

    (* The path-constraint under which the insecurty formula must be
       satifsaible *)
    pc : Formula.bl_term option;
  }

  let empty =
    let check_list = Insec.empty
    and pc = None
    in { check_list; pc }

  let check_to_formula rexpr = Rel_expr.apply_lr Formula.mk_bv_distinct rexpr

  let add_checks t add_checks  =   
    { t with check_list = add_checks t.check_list }

  (* Add the negation of the insecurity checks to ps *)
  let untaint_checks ps list =
    function
    | Secure ->
      (* Check is secure, we do not have to add it to the formula
           but we have to untaint it in ps. *)
      Path_state.untaint ps list
    | Insecure | Unknown as status ->
      match Relse_options.InstrLeak with
      | HaltLeak -> failwith "Should not happen"
      | InstrLeak ->
        (* Do not constraint the insecurity check for the rest of
           the execution *) ps
      | UniqueLeaks when status = Unknown -> ps
      | UniqueLeaks ->
        (* Constraint the insecurity check for the rest of the execution *)
        (* Add hypothesis [not insec] to [ps] *)
        let ps = List.fold_left
            (fun ps check -> Path_state.add_assertion ps
                (check_to_formula check |> Formula.mk_bl_not))
            ps list in
        Path_state.untaint ps list
  
  (** Call the solver to check if the insecurity formula is satisfiable  *)
  let call_solver ~retire_stl qtype asserts ps =
    match Solver.check_sat_formulas ~retire_stl qtype asserts ps with
    | Formula.UNSAT, _ ->
      Logger.debug ~level:2 "[Insecurity][Checked] The insecurity query is unsatisfiable";
      Secure
    | Formula.SAT, ps ->
      (* Handle satisfiable insecurity queries TODO: move to relse.ml *)
      let print_insecurity model msg =
        Logger.result "[Insecurity][Violation] %s" msg;
        Logger.result "Address %a" Path_state.pp_loc ps;
        match model with
        | Some(model) -> Logger.result "@[<v 0>Time: %f| Model:@ %a@]"
                           (Relse_stats.get_time ())
                           Smt_model.pp model;
        | None -> ()
      in
      let exit_se () =
        Relse_stats.add_path ();
        Relse_stats.print_stats ();
        Path_state.pp_path ps max_int;
        exit (Relse_stats.get_exit_code ())
      in
      let model =
        if Relse_options.PrintModel.get ()
        then Some (Solver.get_model_formulas ~retire_stl asserts ps)
        else None
      in 
      let msg = match qtype with
        | Relse_stats.Memory_Insecurity -> "Insecure memory access";
        | Relse_stats.Control_Insecurity -> "Insecure jump";
        | Relse_stats.Insecurity -> "Insecurity query is satisfiable";
        | _ -> failwith "Undefined violation";
      in
      print_insecurity model msg;
      if Relse_options.LeakInfo.get () == HaltLeak then exit_se ();  
      Insecure
    | _ ->
      Logger.warning "[Insecurity][Unknown] at address %a" Path_state.pp_loc ps;
      Unknown
  

  (** Creates an insecurity formula from a relational expression to check *)
  let mk_insec_formula insec_list =
    let append_check_to_formula f rexpr =
      Formula.mk_bl_or f (check_to_formula rexpr)
    in
    List.fold_left append_check_to_formula Formula.mk_bl_false insec_list

  let transient_insec_list insec =
    List.rev_append (Insec.get_cond insec) @@ Insec.get_load insec

  let regular_insec_list insec =
    List.rev_append (Insec.get_cond insec) @@
    List.rev_append (Insec.get_store insec) @@ Insec.get_load insec

  let store_only_insec_list insec =
    Insec.get_store insec

  let mk_insecurity check_list = function
    | Relse_utils.Unsat -> None
    | Relse_utils.Valid ->
      if check_list = [] then None
      else Some (check_list |> mk_insec_formula, check_list)
    | Relse_utils.Sat pc | Relse_utils.Unknown pc ->
      if check_list = [] then None
      else
        let insec = check_list |> mk_insec_formula in
        Some (Formula.mk_bl_and pc insec, check_list)

  
  (** Check that the insecurity formula is not satisfiable *)
  let solve qtype ps t =
    (* Check transient execution *)
    let check ~retire_stl ps = function
      | Some (fml, untaint_list) ->
        let result = call_solver ~retire_stl qtype [fml] ps in
        untaint_checks ps untaint_list result
      | None -> ps
    in
    match Path_state.speculative_mode ps with
    | Relse_path.Spectre.TransientMode ->
      begin
        match transient_insec_list t.check_list with
        | [] -> ps                  (* Nothing to check *)
        | check_list ->
          check ~retire_stl:false ps @@
          mk_insecurity check_list (Path_state.get_transient_pc ps)
      end
    | Relse_path.Spectre.RegularMode ->
      begin
        match regular_insec_list t.check_list with
        | [] -> ps                  (* Nothing to check *)
        | check_list ->
          check ~retire_stl:true ps @@
          mk_insecurity check_list (Path_state.get_regular_pc ps)
      end
    | Relse_path.Spectre.HauntedMode ->
      (* Check loads and control-flow insecurity queries *)
      let ps = match transient_insec_list t.check_list with
        | [] -> ps                  (* Nothing to check *)
        | check_list ->
          check ~retire_stl:false ps @@
          mk_insecurity check_list (Path_state.get_transient_pc ps) in
      (* Check stores in regular execution only *)
      match store_only_insec_list t.check_list with
      | [] -> ps                  (* Nothing to check *)
      | check_list ->
        check ~retire_stl:true ps @@
        mk_insecurity check_list (Path_state.get_regular_pc ps)
end

module Insecurity_State :
sig
  type t
  val create : unit -> t
  val add_memory_check : Relse_path.Path_state.t -> Dba.Instr.t -> t -> t
  val add_cf_check : Rel_expr.rel_bv -> t -> t
  val check_insecurity : level -> Relse_path.Path_state.t -> t -> (Relse_path.Path_state.t * t)
end = struct

  type t =
    | DummyIS
    | InstrIS of Insecurity_formula.t
    | BlockIS of Insecurity_formula.t
  
  let create () =
    if (Relse_options.SymbolicStore.get()) = Sse then
      begin
        Logger.debug ~level:2 "[Insecurity][Init] Dummy insecurity checker";
        DummyIS
      end
    else
      match Relse_options.FaultPacking.get () with
      | 0 -> Logger.debug ~level:2 "[Insecurity][Init] Dummy insecurity checker";
        DummyIS
      | 1 -> Logger.debug ~level:2 "[Insecurity][Init] Instruction level insecurity checker";
        InstrIS (Insecurity_formula.empty)
      | 2 -> Logger.debug ~level:2 "[Insecurity][Init] Block level insecurity checker";
        BlockIS (Insecurity_formula.empty)
      | n -> failwith ("Wrong value for parameter fp=" ^ string_of_int n)

  let add_memory_check ps instr =
    let add_memory_check f =
      let add_checks = Eval_dba.add_memory_accesses instr ps in
      Insecurity_formula.add_checks f add_checks
    in
    function
    | DummyIS -> DummyIS
    | InstrIS f -> InstrIS (add_memory_check f)
    | BlockIS f -> BlockIS (add_memory_check f)

  let add_cf_check r_expr =
    Logger.debug ~level:2 "[Insecurity][add_cf_check] %s"
      (Rel_expr.to_string Formula_pp.print_bv_term r_expr);
    let add_checks insec = Insec.add_cond insec r_expr in
    let add_cf_check f = Insecurity_formula.add_checks f add_checks in
    function
    | DummyIS -> DummyIS
    | InstrIS f -> InstrIS (add_cf_check f)
    | BlockIS f -> BlockIS (add_cf_check f)

  let check_insecurity_aux qtype ps f =
    let do_check () = 
      let ps = Insecurity_formula.solve qtype ps f in
      ps, (Insecurity_formula.empty)
    in
    match Relse_options.LeakInfo.get () with
    | InstrLeak ->
      (* If an insecurity query has already been found at this address, then abort *)
      let insecure_addresses = Relse_stats.get_insecurity_addresses () in
      let current_stm = Path_state.get_current_statement ps in
      if Relse_utils.AddressList.find current_stm insecure_addresses then        
        begin
          Relse_stats.add_spared_check ();
          Logger.debug ~level:2 "[Insecurity][spared] %a already in the list"
            Virtual_address.pp
            Dba_types.(Caddress.to_virtual_address (Statement.location current_stm));
          (* Drop current insecurity checks *)
          ps, (Insecurity_formula.empty)
        end
      else do_check ()
    | _ -> do_check ()
 
  let check_insecurity level ps =
     function
    | DummyIS -> ps, DummyIS
    | InstrIS f ->
      let qtype = match level with
        | Block -> Relse_stats.Control_Insecurity
        | _ -> Relse_stats.Memory_Insecurity in
      let ps, f = check_insecurity_aux qtype ps f in
      ps, InstrIS f
    | BlockIS f when level = Block || level = Path ->
      let qtype = Relse_stats.Insecurity in
      let ps, f = check_insecurity_aux qtype ps f in
      ps, BlockIS f
    | BlockIS f -> (* Do nothing *) ps, BlockIS f 
  
end
