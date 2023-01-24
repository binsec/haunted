open Relse_options

module Path = Relse_utils.AddressList
module Sym_state =  Relse_symbolic.State

type 'a status_t = Continue of 'a | Halt

let decode vaddress = Disasm_core.decode vaddress |> fst

module Spectre =
struct

  type mode = TransientMode
            | RegularMode (* Non transient execution *)
            | HauntedMode
  
  (* Models transient and non transient paths at the same time *)
  module HauntedSE =
  struct
    (* A speculative path constraint *)
    type speculative_pc = {
      retire_depth : int;   (* depth at which the pc must be retired *)
      rel_pc : Formula.bl_term Rel_expr.t; (* path constraint to retire *)
    }

    module PCHeap =
      CCHeap.Make_from_compare(struct
        type t = speculative_pc
        let compare e e' =
          let c = compare e.retire_depth e'.retire_depth in
          if c <> 0 then c else compare e.rel_pc e'.rel_pc
      end)

    type t = {
      (* Queue of transient path constraints to retire later. *)
      speculative_pcs: PCHeap.t;
    }

    let empty = {
      speculative_pcs = PCHeap.empty;
    }

    let maybe_retire t current_depth_opt =
      let pc_update cond = function
        | Some pc -> Some (Rel_expr.apply2 Formula.mk_bl_and pc cond)
        | None -> Some cond
      in
      let rec maybe_retire' speculative_pcs pc =
        match PCHeap.find_min speculative_pcs with
        | Some { retire_depth; rel_pc } ->
          (match current_depth_opt with
          | Some current_depth when retire_depth <= current_depth ->
            let speculative_pcs, _ = PCHeap.take_exn speculative_pcs in
            Logger.debug ~level:7 "[Spectre] Retire condition %a (reached %d)"
              (Rel_expr.pp_rexpr Formula_pp.pp_bl_term) rel_pc current_depth;
           maybe_retire' speculative_pcs (pc_update rel_pc pc)
          | Some _ -> speculative_pcs, pc (* Nothing to retire *)
          | None ->
            (* Maybe retire other conditions *)
           let speculative_pcs, _ = PCHeap.take_exn speculative_pcs in
           Logger.debug ~level:7 "[Spectre] Retire condition %a (fence)"
             (Rel_expr.pp_rexpr Formula_pp.pp_bl_term) rel_pc;
            maybe_retire' speculative_pcs (pc_update rel_pc pc))
        | None -> speculative_pcs, pc (* Nothing to retire *)
      in
      let speculative_pcs, pc = maybe_retire' t.speculative_pcs None in
      { speculative_pcs }, pc

    let start_transient_path ~regular_cond ~retire_depth t =
      (* Put regular condition in the sequence of conditionals to retire *)
      let speculative_pc = { rel_pc=regular_cond; retire_depth } in
      let speculative_pcs = PCHeap.add t.speculative_pcs speculative_pc in
      (* Update the (shadow) regular path condition *)
      { speculative_pcs }

    let speculative_mode t =
      if PCHeap.size t.speculative_pcs <> 0 then
        HauntedMode
      else
        RegularMode

    let regular_pc t current_pc =
      PCHeap.fold
        (fun current_pc speculative_pc  ->
           Relse_utils.update_pc speculative_pc.rel_pc current_pc)
         current_pc t.speculative_pcs
          
    let transient_pc _ current_pc = current_pc

  end

  let update_retire_depth old_retire_depth new_retire_depth =
    match old_retire_depth with
    (* Keep old retire_depth *)
    | Some old_retire_depth ->
      Some (min new_retire_depth old_retire_depth)
    | _ -> Some new_retire_depth
  
  module ExplicitSE =
  struct
    type t = {
      retire_depth : int option;
    }

    let empty = { retire_depth = None }

    let maybe_retire t current_depth_opt =
      match t.retire_depth, current_depth_opt with
      (* Max speculation depth has been reached: kill path *)
      | Some retire_depth, Some current_depth when retire_depth <= current_depth ->
        Logger.debug ~level:7 "[Spectre] Retire conditional (retire \
                               depth = %d) (current depth = %d)"
          retire_depth current_depth; Halt
      | Some retire_depth, None ->
        Logger.debug ~level:7 "[Spectre] Retire conditional (retire \
                               depth = %d) (fence)"
          retire_depth; Halt
      | _ -> Continue t
      
    let start_transient_path t ~retire_depth =
      { retire_depth = update_retire_depth t.retire_depth retire_depth }
    
    let speculative_mode t =
      match t.retire_depth with
      | None -> RegularMode
      | Some _ -> TransientMode

    let regular_pc t current_pc =
      if speculative_mode t = RegularMode
      then current_pc
      else failwith "Cannot get regular_pc in transient execution"
      
    let transient_pc t current_pc =
      if speculative_mode t = TransientMode
      then current_pc
      else failwith "Cannot get transient_pc in regular execution"
  end

  type spectre_pht = NoPHT
                   | ExplicitPHT of ExplicitSE.t
                   | HauntedPHT of HauntedSE.t

  module ExplicitSTL =
  struct
    type t = {
      retire_depth : int option;
    }

    let empty = { retire_depth = None }

    let maybe_retire t current_depth_opt =
      match t.retire_depth, current_depth_opt with
      (* Max speculation depth has been reached: kill path *)
      | Some retire_depth, Some current_depth when retire_depth <= current_depth ->
        Logger.debug ~level:7 "[Spectre] Retire transient load value"; Halt
      | Some _, None ->
        Logger.debug ~level:7 "[Spectre] Retire transient load value (fence)"; Halt
      | _ -> Continue t

    let start_transient_load t ~retire_depth =
      { retire_depth = update_retire_depth t.retire_depth retire_depth }

    let is_transient t =
      match t.retire_depth with
      | None -> false
      | Some _ -> true
  end

  type spectre_stl = NoSTL | ExplicitSTL of ExplicitSTL.t | HauntedIte
  
end

module Shadow_stack = Fstack.Make(Virtual_address)
module Path_state =
struct
  
  type t  = {
    path : Path.t option;
    instruction : Instruction.t;
    block_index : int;
    depth: int;
    shadow_stack : Shadow_stack.t;

    spectre_stl : Spectre.spectre_stl;
    spectre_pht : Spectre.spectre_pht;
    symbolic_state : Sym_state.t;
  }

  let create
      ?(depth=0) ?(path=Some (Path.create ~name:"path"))
      ?(block_index=0)
      symbolic_state instruction =
    assert(block_index >= 0 &&
           block_index <= Dhunk.length instruction.Instruction.dba_block);
    let shadow_stack = Shadow_stack.empty in
    let spectre_pht = match SpectrePht.get () with
      | NoPHT -> Spectre.NoPHT
      | Explicit -> Spectre.ExplicitPHT Spectre.ExplicitSE.empty
      | ExplicitSmarter -> Spectre.ExplicitPHT Spectre.ExplicitSE.empty
      | Haunted -> Spectre.HauntedPHT (Spectre.HauntedSE.empty)
    in
    let spectre_stl = (match SpectreStl.get () with
        | Relse_options.NoSTL -> Spectre.NoSTL
        | Relse_options.Explicit -> Spectre.ExplicitSTL Spectre.ExplicitSTL.empty
        | Relse_options.HauntedIte -> Spectre.HauntedIte)
    in
    { depth; path; symbolic_state; instruction; block_index;
      shadow_stack; spectre_stl; spectre_pht; }
  
  let virtual_address ps =
    let open Instruction in
    ps.instruction.address

  let location st =
    let caddress =
      virtual_address st |> Dba_types.Caddress.of_virtual_address in
    Dba_types.Caddress.reid caddress st.block_index

    (* Accessors to symbolic_state *)
  let formula ~retire_stl ps =
    match ps.spectre_stl with
    | Spectre.ExplicitSTL stl when
        retire_stl &&
        Spectre.ExplicitSTL.is_transient stl ->
      failwith "Cannot ignore transient STL values in ExplicitSTL"
    | Spectre.NoSTL | Spectre.HauntedIte | Spectre.ExplicitSTL _ ->
      Sym_state.formula ~retire_stl ~current_depth:ps.depth ps.symbolic_state

  let path_constraint ps =
    Sym_state.path_constraint ~current_depth:ps.depth ps.symbolic_state

  let var_load ps = Sym_state.var_load ~current_depth:ps.depth ps.symbolic_state

  let memory_select ps size index =
    Sym_state.memory_select ~vaddr:(virtual_address ps)
      ~current_depth:ps.depth ps.symbolic_state size index

  let untaint ps list = 
    let symbolic_state = List.fold_left
        (fun symstate bv -> Sym_state.untaint_bv bv symstate)
        ps.symbolic_state list in
    { ps with symbolic_state }

  let add_assertion ps fml =
    let symbolic_state = Sym_state.add_assertion fml ps.symbolic_state in
    { ps with symbolic_state }

  let declare_high name sort ps =
    let symbolic_state = Sym_state.declare_high  name sort ps.symbolic_state in
    { ps with symbolic_state } 
    
  let declare_low name sort ps =
    let symbolic_state = Sym_state.declare_low  name sort ps.symbolic_state in
    { ps with symbolic_state } 

  let depth ps = ps.depth

  let get_instruction ps =
    ps.instruction

  let get_block_index ps =
    ps.block_index

  let get_dba_instruction ps =
    let block = ps.instruction.Instruction.dba_block in
    Dhunk.inst block ps.block_index |> Utils.unsafe_get_opt

  let assign ?(init=false) value ps =
    let symbolic_state = 
      match value with
      | Relse_utils.Var (name, size, r_val) ->
        Sym_state.var_assign ps.symbolic_state name size r_val
      | Relse_utils.Mem (size, r_index, r_val) ->
        let retire_depth = (match SpectreStl.get () with
            | NoSTL -> ps.depth
            | Explicit | HauntedIte -> ps.depth + SpeculativeWindow.get ()) in
        Sym_state.memory_store ~init ~vaddr:(virtual_address ps)
          ps.symbolic_state ~current_depth:ps.depth ~retire_depth size r_index r_val
    in { ps with symbolic_state }

  let update_pc ?(checked=false) r_cond ps =
    let symbolic_state = Sym_state.pc_update ~checked ps.symbolic_state r_cond in
    { ps with symbolic_state }

  let update_pc_dynamic ?(checked=false) r_expr ps =
    update_pc ~checked r_expr ps

  (* let add_conditional_to_cfg ~true_target ~false_target ps =
   *   let true_caddr =
   *     match true_target with
   *     | Dba.JInner idx -> Dba_types.Caddress.reid (location ps) idx 
   *     | Dba.JOuter addr -> addr in
   *   let false_caddr = Dba_types.Caddress.reid (location ps) false_target in
   *   Relse_cfg.end_basic_block
   *     ~assert_unique:false
   *     ~addr:(Instruction.get_caddress ps.instruction)
   *     ~successors:[true_caddr; false_caddr]
   *     ps.cfg
   *     
   * let add_djump_to_cfg targets ps =
   *   Relse_cfg.end_basic_block
   *     ~assert_unique:true
   *     ~addr:(Instruction.get_caddress ps.instruction)
   *     ~successors:(List.map (fun bv ->
   *         Dba_types.Caddress.of_virtual_address
   *           (Virtual_address.of_bitvector bv))
   *         targets)
   *     ps.cfg
   * 
   * let next_targets_from_cfg ps =
   *   Relse_cfg.get_jump_targets
   *     ~addr:(Instruction.get_caddress ps.instruction)
   *     ps.cfg *)
  
  let update_pc_cond ?(checked=false) condition ps =
    update_pc ~checked condition ps

  let with_init_mem_at ~addr ~size t =
    let symbolic_state = Sym_state.init_mem_at t.symbolic_state ~addr ~size in
    { t with symbolic_state }

  let address_belongs_to_init ~addr ps =
    Sym_state.address_belongs_to_init ~addr ps.symbolic_state

  (** Get the current statement (Caddr + DBA instuction) *)
  let get_current_statement st =
    get_dba_instruction st
    |> Dba_types.Statement.create (location st)

  let goto address ps =
    (* let prev = location st in *)
    let vaddr = Dba_types.Caddress.to_virtual_address address in
    Relse_stats.add_dba_instruction ();
    let instruction, depth =
      if Virtual_address.compare vaddr (virtual_address ps) <> 0 then
        begin
          (* New x86 instruction *)
          Relse_stats.add_instruction vaddr;
          decode vaddr, ps.depth + 1
        end
      else
        ps.instruction, ps.depth
    in
    let block_index = address.Dba.id in
    let ps = { ps with instruction; block_index } in
    let statement = get_current_statement ps in
    let path = match ps.path with
      | None -> None
      | Some x -> Some (Path.extend statement x)
    in
    { ps with path; depth; }
  
  let goto_vaddr address ps =
    goto (Dba_types.Caddress.of_virtual_address address) ps 

  let set_block_index idx st =
    goto (Dba_types.Caddress.reid (location st) idx) st 

  let push_ret addr ps =
    { ps with shadow_stack = Shadow_stack.push addr ps.shadow_stack }

  let pop_ret ps =
    let addr, shadow_stack = Shadow_stack.pop ps.shadow_stack in
    addr, { ps with shadow_stack }
  
  (** {2 Printers} *)

  (** [pp_path ps] Pretty print the current location of [ps] *)
  let pp_loc ppf st =
    let dba_instruction = get_dba_instruction st in
    let vaddress = virtual_address st in
    Format.fprintf ppf "(%a, %d)@ :@ @[%a@]"
      Virtual_address.pp vaddress
      st.block_index
      Dba_printer.Ascii.pp_instruction dba_instruction

  (** [pp_path ps path_number] Pretty print the path leading to [ps]*)
  let pp_path ps path_number = match ps.path with
    | None -> ()
    | Some x -> Path.pp_address_trace_to_file x path_number

  let mark_sat ps =
    let symbolic_state = Sym_state.mark_sat ps.symbolic_state in
    { ps with symbolic_state }

  let mark_unsat ps =
    let symbolic_state = Sym_state.mark_unsat ps.symbolic_state in
    { ps with symbolic_state }

  let mark_unknown ps =
    let symbolic_state = Sym_state.mark_unknown ps.symbolic_state in
    { ps with symbolic_state }

  let maybe_add_comment ps comment =
    if Sse_options.Comment.get () then
      let symstate = Sym_state.comment comment ps.symbolic_state in
      { ps with symbolic_state = symstate }
    else ps      

  (* Spectre *)
  let maybe_retire_pht ps current_depth_opt =
    match ps.spectre_pht with
    | Spectre.NoPHT -> Continue ps
    | Spectre.ExplicitPHT pht ->
      (match Spectre.ExplicitSE.maybe_retire pht current_depth_opt with
       | Continue pht ->
         Continue { ps with spectre_pht = (Spectre.ExplicitPHT pht) }
       | Halt -> Halt)
    | Spectre.HauntedPHT pht ->
      (match Spectre.HauntedSE.maybe_retire pht current_depth_opt with
       | pht, Some rel_pc ->
         let ps = update_pc_cond rel_pc ps in
         Continue { ps with spectre_pht = (Spectre.HauntedPHT pht) }
       | pht, None ->
         Continue { ps with spectre_pht = (Spectre.HauntedPHT pht) })

  let maybe_retire_stl ps current_depth_opt =
    match ps.spectre_stl with
    | Spectre.ExplicitSTL stl ->
      (match Spectre.ExplicitSTL.maybe_retire stl current_depth_opt with
       | Continue spectre_stl ->
         Continue { ps with spectre_stl = Spectre.ExplicitSTL spectre_stl }
       | Halt -> Halt)
    | Spectre.HauntedIte ->
      let symbolic_state = Sym_state.fence ps.symbolic_state in
      Continue { ps with symbolic_state }
    | Spectre.NoSTL -> Continue ps
  
  let maybe_retire ps =
    match maybe_retire_pht ps (Some ps.depth) with
    | Continue ps ->
      (match maybe_retire_stl ps (Some ps.depth) with
       | Continue ps -> Continue ps
       | Halt -> Halt)
    | Halt -> Halt

  let fence ps =
    match maybe_retire_pht ps None with
    | Continue ps ->
      (match maybe_retire_stl ps None with
       | Continue ps -> Continue ps
       | Halt -> Halt)
    | Halt -> Halt

    (* let transient_pht_mode ps =
     *   match ps.spectre with
     *   | Spectre.NoSpectre -> Spectre.RegularMode
     *   | Spectre.Explicit spectre -> Spectre.ExplicitSE.speculative_mode spectre
     *   | Spectre.Haunted spectre -> Spectre.HauntedSE.speculative_mode spectre *)

  let speculative_mode ps =
    match ps.spectre_stl with
    | Spectre.ExplicitSTL stl when Spectre.ExplicitSTL.is_transient stl ->
      Spectre.TransientMode
    | Spectre.HauntedIte ->
      Spectre.HauntedMode
    | _ -> match ps.spectre_pht with
      | Spectre.NoPHT -> Spectre.RegularMode
      | Spectre.ExplicitPHT pht -> Spectre.ExplicitSE.speculative_mode pht
      | Spectre.HauntedPHT pht -> Spectre.HauntedSE.speculative_mode pht

  let get_transient_pc ps =
    let fml = path_constraint ps in
    match ps.spectre_pht with
    | Spectre.NoPHT -> fml
    | Spectre.ExplicitPHT pht ->
      Spectre.ExplicitSE.transient_pc pht fml 
    | Spectre.HauntedPHT pht ->
      Spectre.HauntedSE.transient_pc pht fml

  let get_regular_pc ps =
    let fml = path_constraint ps in
    match ps.spectre_pht with
    | Spectre.NoPHT -> fml
    | Spectre.ExplicitPHT pht ->
      Spectre.ExplicitSE.regular_pc pht fml
    | Spectre.HauntedPHT pht ->
      Spectre.HauntedSE.regular_pc pht fml

  (* You must add the conditional before execution this function *)
  let start_explicit_transient_path ~retire_depth ps =
    Logger.debug ~level:2 "[Spectre] Starting explicit transient execution";
    match ps.spectre_pht with
    | Spectre.NoPHT ->
      failwith "No transient paths in NoSpectre SE"
    | Spectre.ExplicitPHT pht ->
      let spectre_pht = Spectre.ExplicitSE.start_transient_path pht ~retire_depth in
      { ps with spectre_pht = Spectre.ExplicitPHT spectre_pht }
    | Spectre.HauntedPHT _ ->
      failwith "Cannot start explicit transient path with Shadow model."

  (* You must add the conditional before execution this function *)
  let start_haunted_path ~retire_depth ~transient_cond ~regular_cond ps =
    Logger.debug ~level:2 "[Spectre] Speculative execution of %a"
      (Rel_expr.pp_rexpr Formula_pp.pp_bl_term) transient_cond;
    match ps.spectre_pht with
    | Spectre.NoPHT ->
      failwith "No transient paths in NoSpectre SE"
    | Spectre.ExplicitPHT _ ->
      failwith "Cannot start shadow transient path with Explicit model."
    | Spectre.HauntedPHT pht ->
      let spectre_pht = Spectre.HauntedSE.start_transient_path
          ~regular_cond ~retire_depth  pht in
      { ps with spectre_pht = Spectre.HauntedPHT spectre_pht }

  let start_transient_load ps ~retire_depth =
    match ps.spectre_stl with
    | Spectre.ExplicitSTL stl -> 
      let spectre_stl = Spectre.ExplicitSTL.start_transient_load
          stl ~retire_depth in
      { ps with spectre_stl = Spectre.ExplicitSTL spectre_stl }
    | _ -> failwith "Cannot start transient load when SpectreSTL is \
                        not in explicit mode."

end
