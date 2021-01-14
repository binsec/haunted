open Relse_options

module AL = Relse_utils.AddressList
module AS = Relse_utils.AddressSet

(* Exit code *)
type exit_code = Secure | Insecure | Unknown

let exit_code_to_int = function
  | Secure -> 0
  | Insecure -> 7
  | Unknown -> 8

let exit_code_to_string = function
  | Secure -> "Secure"
  | Insecure -> "Insecure"
  | Unknown -> "Unknown"

(* Status *)
module Status = struct
  type status = Violation | Max_Depth | Max_Paths | Unsupported_Syscall
              | Solver_Timeout | Timeout | EnumLimit | Interrupt | OOM
  type t = {
    violations : int;
    max_depth : int;
    max_paths : int;
    syscalls : int;
    solver_timeout : int;
    enum_limit : int;
    timeout : int;
    interrupt : int;
    oom : int;
  }

  let initial_status = {
    violations = 0;
    max_depth  = 0;
    max_paths = 0;
    syscalls = 0;
    solver_timeout = 0;
    enum_limit = 0;
    timeout = 0;
    interrupt = 0;
    oom = 0;
  }

  let csv_header =
    "violations,max_depth_reached,max_paths_reached,syscalls," ^
    "solver_timeout_reached,enum_limit_reached,timeout_reached," ^
    "interrupted,out_of_memory,"

  let print_status_csv fmt t =
    Format.fprintf fmt "%d,%d,%d,%d,%d,%d,%d,%d,%d,"
      t.violations t.max_depth t.max_paths t.syscalls t.solver_timeout
      t.enum_limit t.timeout t.interrupt t.oom

  let print_status fmt t =
    Format.fprintf fmt "Status: @[<v 2>@ \
                        Violations:\t%d@ \
                        Max Depth:\t%d@ \
                        Max Paths:\t%d@ \
                        Syscalls:\t%d@ \
                        Solver Timeout:\t%d@ \
                        Enum limit:\t%d@ \
                        Global Timeout:\t%d@ \
                        Interrupt:\t%d@ \
                        Out of Memory:\t%d@]@\n"
      t.violations t.max_depth t.max_paths t.syscalls t.solver_timeout
      t.enum_limit t.timeout t.interrupt t.oom

  let get_exit_code t =
    if t.violations > 0
    then Insecure
    else if
      t.max_depth = 0 && t.max_paths = 0 && t.syscalls = 0 && t.solver_timeout = 0 &&
      t.enum_limit = 0 && t.timeout = 0 && t.interrupt = 0 && t.oom = 0
    then Secure
    else Unknown

  let set_status t = function
    | Violation -> { t with violations = t.violations + 1 }      
    | Max_Depth -> { t with max_depth = t.max_depth + 1 }
    | Max_Paths -> { t with max_paths = t.max_paths + 1 }
    | Unsupported_Syscall -> { t with syscalls = t.syscalls + 1 }
    | Solver_Timeout -> { t with solver_timeout = t.solver_timeout + 1 }
    | EnumLimit -> { t with enum_limit = t.enum_limit + 1 }
    | Timeout -> { t with timeout = t.timeout + 1 }
    | Interrupt -> { t with interrupt = t.interrupt + 1 }
    | OOM -> { t with oom = t.oom + 1 }
end
  
type query_type = Exploration | Control_Insecurity | Memory_Insecurity | Insecurity | Model | Enum

type query_record = {
  total : int;
  sat   : int;
  unsat : int;
  err   : int;
  time  : float ;
}

let empty_query_record = {
  total = 0;
  sat = 0;
  unsat = 0;
  err = 0;
  time = 0.0;
}

let print_query_record_csv fmt qr =
  Format.fprintf fmt "%d,%d,%d,%d,%f,"
    qr.sat
    qr.unsat
    qr.err
    qr.total
    qr.time

let print_query_record fmt label qr =
  Format.fprintf fmt "%s: @[<v 2>@ \
                      SAT:\t%d@ UNSAT:\t%d@ Other:\t%d@ Total:\t%d@ Time:\t%f@ Time avg:\t%f@]@\n"
    label
    qr.sat
    qr.unsat
    qr.err
    qr.total
    qr.time
    (qr.time /. (float_of_int qr.total))

let get_solver () =
  Formula_options.(
    match Solver.get () with
    | Boolector -> "boolector"
    | Z3 -> "z3"
    | CVC4 -> "cvc4"
    | Yices -> "yices")
    
let get_store () =
  match SymbolicStore.get () with
  | Sse -> "sse"
  | SelfComposed -> "self-comp"
  | Relational -> "relational"

let get_mem () =
  match MemoryType.get () with
  | MemStd -> "std"
  | MemList -> "row-list"
  | MemMap -> "row-map"

type t = {
  exploration_queries : query_record ref;
  control_insecurity_queries : query_record ref;
  memory_insecurity_queries : query_record ref;
  insecurity_queries : query_record ref;
  model_queries : query_record ref;
  enum_queries : query_record ref;
  
  insecurity_checks : int * int;

  total_query_size : int;
  max_query_size : int;
  
  paths : int;
  conditional_instructions : int;
  forks : int;
  resolved_forks : int;
  unresolved_forks : int;
  discarded: int;
  instructions : int;
  dba_instructions : int;
  enum_limit_reached : int;
  min_transient_dj : int;

  merged_paths : int;
  refused_mergers : int;
  aborted_mergers : int;

  start_time : float;
  status : Status.t;

  instructions_explored : AS.t;
  exploration_query_addr : AL.t;
  insecure_query_addr : AL.t;
  secure_query_addr : AL.t;
  timeout_query_addr : AL.t;

  (* Spectre *)
  transient_paths : int;
  transient_loads : int;
}

let empty = {
  exploration_queries = ref empty_query_record;
  control_insecurity_queries = ref empty_query_record;
  memory_insecurity_queries = ref empty_query_record;
  insecurity_queries = ref empty_query_record;
  model_queries = ref empty_query_record;
  enum_queries = ref empty_query_record;
  
  insecurity_checks = (0,0);

  total_query_size = 0;
  max_query_size = 0;
  
  paths = 0;
  conditional_instructions = 0;
  forks = 0;
  resolved_forks = 0;
  unresolved_forks = 0;
  discarded = 0;
  instructions = 0;
  dba_instructions = 0;
  enum_limit_reached = 0;
  min_transient_dj = 0;
  
  merged_paths = 0;
  refused_mergers = 0;
  aborted_mergers = 0;
  
  start_time = Unix.gettimeofday();
  status = Status.initial_status;

  instructions_explored = AS.create ~name:"coverage";
  exploration_query_addr = AL.create ~name:"explor";
  insecure_query_addr = AL.create ~name:"insecure";
  secure_query_addr = AL.create ~name:"secure";
  timeout_query_addr = AL.create ~name:"timeout";

  transient_paths = 0;
  transient_loads = 0;
}

let stat = ref empty

let get_total () =
  let exploration_queries = !(!stat.exploration_queries)
  and insecurity_queries = !(!stat.insecurity_queries)
  and model_queries = !(!stat.model_queries)
  and enum_queries = !(!stat.enum_queries) in
  let total = exploration_queries.total + insecurity_queries.total + model_queries.total + enum_queries.total
  and sat = exploration_queries.sat + insecurity_queries.sat + model_queries.sat + enum_queries.sat
  and unsat = exploration_queries.unsat + insecurity_queries.unsat + model_queries.unsat + enum_queries.unsat
  and err = exploration_queries.err + insecurity_queries.err + model_queries.err + enum_queries.err
  and time = exploration_queries.time +. insecurity_queries.time +. model_queries.time +. enum_queries.time
  in { total; sat; unsat; err; time }

let get_insecurity_addresses () =
  !stat.insecure_query_addr

let add_query_size sz =
  let max_query_size =
    if sz > !stat.max_query_size
    then sz
    else !stat.max_query_size
  and total_query_size = sz + !stat.total_query_size
  in stat := { !stat with max_query_size ; total_query_size }

let update_status result addr query_type =
  let update_insec_query () =
    match result with
    | Formula.SAT ->
      let status = Status.set_status !stat.status Status.Violation in
      let insecure_query_addr = AL.extend addr !stat.insecure_query_addr in
      stat := { !stat with status ; insecure_query_addr }
    | Formula.UNSAT ->
      let secure_query_addr = AL.extend addr !stat.secure_query_addr in
      stat := { !stat with secure_query_addr }
    | Formula.TIMEOUT | Formula.UNKNOWN ->
      let status = Status.set_status !stat.status Status.Solver_Timeout in
      let timeout_query_addr = AL.extend addr !stat.timeout_query_addr in
      stat := { !stat with status; timeout_query_addr }
  in
  let update_exploration_query () =
    let exploration_query_addr = AL.extend addr !stat.exploration_query_addr in
    stat := { !stat with exploration_query_addr }
  in
  match query_type with
  | Exploration -> update_exploration_query ()
  | Control_Insecurity -> update_insec_query ()
  | Memory_Insecurity -> update_insec_query ()
  | Insecurity -> update_insec_query ()
  | Model -> ()
  | Enum -> update_exploration_query ()
  
let add_query time result query_type =
  let update_queries queries =
    queries := {
      unsat = !queries.unsat + if result = Formula.UNSAT then 1 else 0;
      sat   = !queries.sat + if result = Formula.SAT then 1 else 0;
      err   = !queries.err + if result <> Formula.SAT && result <> Formula.UNSAT then 1 else 0;
      total = !queries.total + 1;
      time = !queries.time +. time;
    }
  in
  match query_type with
  | Exploration ->
    update_queries !stat.exploration_queries;
  | Control_Insecurity ->
    update_queries !stat.control_insecurity_queries;
    update_queries !stat.insecurity_queries
  | Memory_Insecurity ->
    update_queries !stat.memory_insecurity_queries;
    update_queries !stat.insecurity_queries
  | Insecurity -> update_queries !stat.insecurity_queries
  | Model -> update_queries !stat.model_queries
  | Enum ->
    update_queries !stat.enum_queries

let add_done_check () =
  let checked = fst !stat.insecurity_checks + 1
  and spared = snd !stat.insecurity_checks in
  stat := { !stat with insecurity_checks = (checked, spared) }
                
let add_spared_check () =
  let checked = fst !stat.insecurity_checks
  and spared = snd !stat.insecurity_checks + 1 in
  stat := { !stat with insecurity_checks = (checked, spared) }

(* let avg_insecurity_length () =
 *   let n = fst !stat.insecurity_length
 *   and sum = snd !stat.insecurity_length in
 *   if n <> 0
 *   then (float_of_int sum) /. (float_of_int n)
 *   else 0.0 *)

let add_path () =
  stat := { !stat with paths = !stat.paths + 1 }

let get_nb_paths () = !stat.paths

let add_transient_path () = 
  stat := { !stat with transient_paths = !stat.transient_paths + 1 }

let add_transient_load () = 
  stat := { !stat with transient_loads = !stat.transient_loads + 1 }

let add_conditional () =
  stat := { !stat with conditional_instructions = !stat.conditional_instructions + 1 }

let add_fork () =
  stat := { !stat with forks = !stat.forks + 1 }

let add_resolved_fork () =
  stat := { !stat with resolved_forks = !stat.resolved_forks + 1 }

let add_unresolved_fork () =
  stat := { !stat with unresolved_forks = !stat.unresolved_forks + 1 }

let discard_path () =
  stat := { !stat with discarded = !stat.discarded + 1 }

let add_instruction vaddr =
  AS.add vaddr !stat.instructions_explored;
  stat := { !stat with instructions = !stat.instructions + 1 }

let add_dba_instruction () =
  stat := { !stat with dba_instructions = !stat.dba_instructions + 1 }

let add_merged_path () =
  stat := { !stat with merged_paths = !stat.merged_paths + 1 }

let add_refused_merger () =
  stat := { !stat with refused_mergers = !stat.refused_mergers + 1 }

let add_aborted_merger () =
  stat := { !stat with aborted_mergers = !stat.aborted_mergers + 1 }

let set_start () =
  stat := { !stat with start_time = Unix.gettimeofday() }

let get_time () =
  (Unix.gettimeofday() -. !stat.start_time)

let pht_status_to_string () =
  match Relse_options.SpectrePht.get () with
  | Relse_options.NoPHT -> "NoPHT"
  | Relse_options.Explicit -> "Explicit"
  | Relse_options.ExplicitSmarter -> "ExplicitSmarter"
  | Relse_options.Haunted -> "Haunted"

let pht_dynamic_to_string () =
  match Relse_options.DynamicPht.get () with
  | Relse_options.Static -> "Static"
  | Relse_options.Hybrid n -> Format.asprintf "Hybrid_%d" n
  | Relse_options.FullyDynamic -> "Full"

let stl_status_to_string () =
  match Relse_options.SpectreStl.get () with
  | Relse_options.NoSTL -> "NoSTL"
  | Relse_options.Explicit -> "ExplicitSTL"
  | Relse_options.HauntedIte -> "HauntedIteSTL"

let get_exit_code () =
  Status.get_exit_code !stat.status
  |> exit_code_to_int

let set_status status =
  let status = Status.set_status !stat.status status in
  stat := { !stat with status }

let enum_limit_reached () =
  set_status Status.EnumLimit;
  let enum_limit_reached = !stat.enum_limit_reached + 1 in
  stat := { !stat with enum_limit_reached }

let add_transient_dynamic_jump () =
  stat := { !stat with min_transient_dj  = !stat.min_transient_dj + 1 }

let pp fmt stat =
  let total_qr = get_total () in
  Format.fprintf fmt "RelSE stats:@[<v 2>@\n";
  print_query_record fmt "Total queries" (total_qr);
  print_query_record fmt "Exploration queries" !(stat.exploration_queries); 
  print_query_record fmt "CF Insecurity queries" !(stat.control_insecurity_queries);
  print_query_record fmt "Mem Insecurity queries" !(stat.memory_insecurity_queries);
  print_query_record fmt "Insecurity queries" !(stat.insecurity_queries);
  print_query_record fmt "Model queries" !(stat.model_queries);
  print_query_record fmt "Enum queries" !(stat.enum_queries);
  print_query_record fmt "Total queries" (total_qr);
  Format.fprintf fmt "Query size avg/max:\t%f / %d@ \
                      Checks done/spared:\t%d / %d@ \
                      Enum limits reached:\t%d@ \
                      Transient dynamic jumps (min):\t%d@ \
                      Coverage: @[<v 2>@ Paths:\t\t%d@ Conditionals:\t%d@ Forks:\t%d(Resolved=%d/Unresolved=%d)@ Discarded paths:\t%d@ DBA Instructions:\t%d@ x86 Instructions:\t%d@]@ \
                      Packing: @[<v 2>@ Merged paths:\t\t%d@ Refused mergers:\t%d@ Aborted mergers:\t%d@]@ \
                      Spectre: @[<v 2>@ Transient paths:\t\t%d@ Transient Loads:\t\t%d@]@ \
                      Elapsed time:\t%f\n \
                      Result:\t%s@"
    (float_of_int stat.total_query_size /. (float_of_int total_qr.total))
    stat.max_query_size
    (fst stat.insecurity_checks)
    (snd stat.insecurity_checks)
    stat.enum_limit_reached
    stat.min_transient_dj
    stat.paths
    stat.conditional_instructions
    stat.forks
    stat.resolved_forks
    stat.unresolved_forks
    stat.discarded
    stat.dba_instructions
    stat.instructions
    stat.merged_paths
    stat.refused_mergers
    stat.aborted_mergers
    stat.transient_paths
    stat.transient_loads
    (get_time ())
    (Status.get_exit_code stat.status |> exit_code_to_string);
  (Status.print_status fmt stat.status);
  Format.fprintf fmt "@]"

let csv_header = "label,fp,dd,untainting,\
                  Explor SAT,Explor UNSAT,Explor other,Explor \
                  total,Explor time,\
                  CF SAT,CF UNSAT,CF other,CF total,CF time,\
                  Mem SAT,Mem UNSAT,Mem other,Mem total,Mem time,\
                  Insec SAT,Insec UNSAT,Insec other,Insec total,Insec \
                  time,\
                  Model total,Model time,\
                  Enum total,Enum time,enum_limit_reached,\
                  Total SAT,Total UNSAT,Total other,Total total,Total \
                  time,\
                  average query size, max query size,\
                  done checks,spared \
                  checks,paths,conditions,forks,unresolved_fork,resolved_forks,discarded,\
                  min_transient_dj,dba_instructions,x86instructions,addresses,\
                  merged,refused,aborted,wall time,\
                  MAX_DEPTH,MAX_PATH,TIMEOUT,store,canonical,mem_type,solver,bopt,\
                  transient_paths_pht,transient_paths_stl,pht_status,pht_dynamic,stl_status,max_spec_depth," ^
                 Status.csv_header ^ "exit_code,\n"

let pp_csv ~with_header ~label ~fp ~dd ~untainting fmt stat =
  let total_qr = get_total () in
  if with_header then
    Format.pp_print_string fmt csv_header;
  Format.fprintf fmt "%s,%d,%d,%d," label fp dd untainting;
  print_query_record_csv fmt !(stat.exploration_queries); 
  print_query_record_csv fmt !(stat.control_insecurity_queries);
  print_query_record_csv fmt !(stat.memory_insecurity_queries);
  print_query_record_csv fmt !(stat.insecurity_queries);
  Format.fprintf fmt "%d,%f,%d,%f,%d,"
  !(stat.model_queries).total
  !(stat.model_queries).time
  !(stat.enum_queries).total
  !(stat.enum_queries).time
  stat.enum_limit_reached;
  print_query_record_csv fmt (total_qr);
  Format.fprintf fmt "%f,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%f,%d,%d,%f,%s,%d,%s,%s,%d,%d,%d,%s,%s,%s,%d,"
    (float_of_int stat.total_query_size /. (float_of_int total_qr.total))
    stat.max_query_size
    (fst stat.insecurity_checks)
    (snd stat.insecurity_checks)
    stat.paths
    stat.conditional_instructions
    stat.forks
    stat.unresolved_forks
    stat.resolved_forks
    stat.discarded
    stat.min_transient_dj
    stat.dba_instructions
    stat.instructions
    (AS.size stat.instructions_explored)
    stat.merged_paths
    stat.refused_mergers
    stat.aborted_mergers
    (get_time ())
    (Sse_options.MaxDepth.get ())
    (Relse_options.MaxPaths.get ())
    (Sse_options.Timeout.get ())
    (get_store ())
    (if Relse_options.Canonical.get () then 1 else 0)
    (get_mem ())
    (get_solver ())
    (if Formula_options.OptimAll.get () then 1 else 0)
    stat.transient_paths
    stat.transient_loads
    (pht_status_to_string ())
    (pht_dynamic_to_string ())
    (stl_status_to_string ())
    (Relse_options.SpeculativeWindow.get ());
  (Status.print_status_csv fmt stat.status);
  Format.fprintf fmt "%s\n" (Status.get_exit_code stat.status |> exit_code_to_string)

  
(* Save address trace *)
let addresses_to_file stat =
  AS.pp_address_trace_to_file stat.instructions_explored 0;
  AL.pp_address_trace_to_file stat.exploration_query_addr 0;
  AL.pp_address_trace_to_file stat.insecure_query_addr 0;
  AL.pp_address_trace_to_file stat.secure_query_addr 0;
  AL.pp_address_trace_to_file stat.timeout_query_addr 0

(** Print the smt statistics at the end of the execution*)
let print_stats () =
  let stats = !stat in
  addresses_to_file stats;
  if Relse_options.StatFile.is_set() then
    let open Unix in
    let fname = Relse_options.StatFile.get() in
    let with_header, file =
      try
        Logger.debug ~level:4 "open %s mode x" fname;
        true, openfile fname [O_WRONLY; O_CREAT; O_EXCL] 0o644
      with Unix_error (EEXIST, _, _) ->
        Logger.debug ~level:4 "open %s mode a" fname;
        false, openfile fname [O_WRONLY; O_APPEND] 0o644
    in
    let out = out_channel_of_descr file in
    let fmt = Format.formatter_of_out_channel out in
    (* Options *)
    let fp = Relse_options.FaultPacking.get()
    and dd = Relse_options.Dedup.get()
    and untainting = (if Untainting.get () then 1 else 0)
    and label = Relse_options.StatPrefix.get() in
    Format.fprintf
      fmt
      "%a"
      (pp_csv ~with_header ~label ~fp ~dd ~untainting) stats;
    Format.pp_print_flush fmt ();
    close_out out
  else
    Logger.info "%a" pp stats
