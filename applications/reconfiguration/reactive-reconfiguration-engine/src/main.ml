open Tempo
open Raylib

type comp_state = Stopped | Running | Updating
type scenario = Baseline | Lossy | Partitioned
type runtime_mode = Normal | Degraded | Recovery

type policy_profile = {
  pname : string;
  max_parallel : int;
  tx_deadline : int;
  auto_period : int;
  rollback_on_failure : bool;
}

type operation =
  | Add_component of string
  | Remove_component of int
  | Bind of int * int
  | Unbind of int * int
  | Start of int
  | Stop of int
  | Update of int

type step_status = Pending of int | Done | Failed of string
type tx_status = Waiting | Running_tx | Committed_tx | Aborted_tx of string | Rolling_back of int | Rolled_back

type tx = {
  id : int;
  label : string;
  steps : operation array;
  step_limits : int option array;
  step_status : step_status array;
  cursor : int;
  status : tx_status;
  compensation : operation list;
  created_at : int;
  deadline : int;
}

type comp = {
  id : int;
  name : string;
  state : comp_state;
}

type event = {
  step : int;
  kind : string;
  detail : string;
}

type template = {
  tname : string;
  tops : (operation * int option) list;
  tdeadline : int option;
}

type op_ast =
  | A_add of string * int option
  | A_remove of string * int option
  | A_bind of string * string * int option
  | A_unbind of string * string * int option
  | A_start of string * int option
  | A_stop of string * int option
  | A_update of string * int option

type template_ast = {
  aname : string;
  aops : op_ast list;
  adeadline : int option;
}

type system_spec = {
  s_components : (string * comp_state) list;
  s_groups : (string * string list) list;
  s_deps : (string * string) list;
  s_bindings : (string * string) list;
  s_policy : policy_profile;
  s_scenario : scenario;
}

type msg =
  | Boot
  | Tick
  | Set_mode of runtime_mode
  | Toggle_partition
  | Loss_up
  | Loss_down
  | Next_policy
  | Next_scenario
  | Queue_next_template
  | Reload_specs

type model = {
  step : int;
  seed : int;
  comps : comp array;
  deps : (int * int) list;
  bindings : (int * int) list;
  link_ttl : int array;
  partition : bool;
  loss_rate : float;
  policy : policy_profile;
  scenario : scenario;
  mode : runtime_mode;
  txs : tx list;
  templates : template list;
  next_template_idx : int;
  next_tx : int;
  commits : int;
  aborts : int;
  rollbacks : int;
  violations : int;
  invariant_ok : bool;
  last_event : string;
  event_log : event list;
  auto_clock : int;
  spec_issues : string list;
  system_file : string;
  reconfig_file : string;
}

let width = 1120
let height = 700

let default_system_file = "applications/reconfiguration/reactive-reconfiguration-engine/spec/system.concerto"
let default_reconfig_file = "applications/reconfiguration/reactive-reconfiguration-engine/spec/reconfig.concerto"

let scenario_name = function Baseline -> "Baseline" | Lossy -> "Lossy" | Partitioned -> "Partitioned"
let mode_name = function Normal -> "normal" | Degraded -> "degraded" | Recovery -> "recovery"

let state_name = function
  | Stopped -> "STOP"
  | Running -> "RUN"
  | Updating -> "UPD"

let op_name = function
  | Add_component n -> "ADD " ^ n
  | Remove_component i -> Printf.sprintf "REMOVE C%d" i
  | Bind (a, b) -> Printf.sprintf "BIND C%d->C%d" a b
  | Unbind (a, b) -> Printf.sprintf "UNBIND C%d->C%d" a b
  | Start i -> Printf.sprintf "START C%d" i
  | Stop i -> Printf.sprintf "STOP C%d" i
  | Update i -> Printf.sprintf "UPDATE C%d" i

let step_status_text = function
  | Pending t -> Printf.sprintf "pending %d" t
  | Done -> "done"
  | Failed r -> "failed " ^ r

let trim s =
  let is_sp = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
  let n = String.length s in
  let i = ref 0 and j = ref (n - 1) in
  while !i < n && is_sp s.[!i] do
    incr i
  done;
  while !j >= !i && is_sp s.[!j] do
    decr j
  done;
  if !j < !i then "" else String.sub s !i (!j - !i + 1)

let split_words s = String.split_on_char ' ' s |> List.map trim |> List.filter (fun x -> x <> "")

let read_lines path =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  loop []

let parse_bool = function "true" | "yes" | "1" -> true | _ -> false

let parse_scenario = function
  | "lossy" -> Lossy
  | "partitioned" -> Partitioned
  | _ -> Baseline

let parse_state = function
  | "running" -> Running
  | "updating" -> Updating
  | _ -> Stopped

let profile_conservative =
  { pname = "conservative"; max_parallel = 1; tx_deadline = 460; auto_period = 340; rollback_on_failure = true }

let profile_balanced =
  { pname = "balanced"; max_parallel = 2; tx_deadline = 320; auto_period = 240; rollback_on_failure = true }

let profile_aggressive =
  { pname = "aggressive"; max_parallel = 3; tx_deadline = 220; auto_period = 170; rollback_on_failure = true }

let profile_from_preset = function
  | "conservative" -> profile_conservative
  | "aggressive" -> profile_aggressive
  | _ -> profile_balanced

let default_system_spec =
  {
    s_components = [ ("Proxy", Running); ("API", Running); ("DB", Running); ("Worker", Stopped) ];
    s_groups = [ ("frontend", [ "Proxy"; "API" ]); ("backend", [ "DB"; "Worker" ]) ];
    s_deps = [ ("API", "DB"); ("Worker", "API"); ("Proxy", "API") ];
    s_bindings = [ ("API", "DB"); ("Proxy", "API") ];
    s_policy = profile_balanced;
    s_scenario = Baseline;
  }

let load_system_spec path =
  if not (Sys.file_exists path) then default_system_spec
  else
    let lines = read_lines path in
    let comps = ref [] in
    let groups = ref [] in
    let deps = ref [] in
    let bindings = ref [] in
    let policy = ref profile_balanced in
    let scenario = ref Baseline in
    List.iter
      (fun raw ->
        let line = trim raw in
        if line <> "" && not (String.length line >= 1 && line.[0] = '#') then
          match split_words line with
          | [ "component"; name; state ] ->
              comps := (name, parse_state (String.lowercase_ascii state)) :: !comps
          | "group" :: gname :: members ->
              groups := (gname, members) :: !groups
          | [ "depends"; child; parent ] ->
              deps := (child, parent) :: !deps
          | [ "bind"; a; b ] ->
              bindings := (a, b) :: !bindings
          | [ "policy"; p ] ->
              policy := profile_from_preset (String.lowercase_ascii p)
          | [ "policy_profile"; name ] ->
              policy := { !policy with pname = String.lowercase_ascii name }
          | [ "max_parallel"; n ] ->
              policy := { !policy with max_parallel = max 1 (int_of_string n) }
          | [ "tx_deadline"; n ] ->
              policy := { !policy with tx_deadline = max 20 (int_of_string n) }
          | [ "auto_period"; n ] ->
              policy := { !policy with auto_period = max 20 (int_of_string n) }
          | [ "rollback_on_failure"; b ] ->
              policy := { !policy with rollback_on_failure = parse_bool (String.lowercase_ascii b) }
          | [ "scenario"; s ] ->
              scenario := parse_scenario (String.lowercase_ascii s)
          | _ -> ())
      lines;
    let components = if !comps = [] then default_system_spec.s_components else List.rev !comps in
    {
      s_components = components;
      s_groups = if !groups = [] then default_system_spec.s_groups else List.rev !groups;
      s_deps = List.rev !deps;
      s_bindings = List.rev !bindings;
      s_policy = !policy;
      s_scenario = !scenario;
    }

let default_reconfig_ast =
  [
    {
      aname = "start-backend";
      aops = [ A_start ("@backend", Some 120); A_bind ("Worker", "API", Some 160) ];
      adeadline = Some 260;
    };
    {
      aname = "api-hot-update";
      aops = [ A_update ("API", Some 100); A_start ("API", Some 140) ];
      adeadline = Some 220;
    };
    {
      aname = "rewire-frontend";
      aops =
        [
          A_unbind ("Proxy", "API", Some 120);
          A_bind ("Proxy", "Worker", Some 170);
          A_start ("Worker", Some 170);
        ];
      adeadline = Some 300;
    };
  ]

let parse_template_header words =
  match words with
  | [ "transaction"; name ] -> Some (name, None)
  | [ "transaction"; name; "deadline"; d ] -> Some (name, Some (int_of_string d))
  | _ -> None

let split_within words =
  match List.rev words with
  | n :: "within" :: rev_base -> (List.rev rev_base, Some (int_of_string n))
  | _ -> (words, None)

let parse_op_line words =
  let base, within = split_within words in
  match base with
  | [ "add"; n ] -> Some (A_add (n, within))
  | [ "remove"; n ] -> Some (A_remove (n, within))
  | [ "bind"; a; b ] -> Some (A_bind (a, b, within))
  | [ "unbind"; a; b ] -> Some (A_unbind (a, b, within))
  | [ "start"; n ] -> Some (A_start (n, within))
  | [ "stop"; n ] -> Some (A_stop (n, within))
  | [ "update"; n ] -> Some (A_update (n, within))
  | _ -> None

let load_reconfig_ast path =
  if not (Sys.file_exists path) then default_reconfig_ast
  else
    let lines = read_lines path in
    let acc = ref [] in
    let current = ref None in
    List.iter
      (fun raw ->
        let line = trim raw in
        if line <> "" && not (String.length line >= 1 && line.[0] = '#') then
          let words = split_words line in
          match !current, words with
          | _, "transaction" :: _ ->
              (match parse_template_header words with
              | Some (name, deadline) -> current := Some { aname = name; aops = []; adeadline = deadline }
              | None -> ())
          | Some t, [ "end" ] ->
              acc := { t with aops = List.rev t.aops } :: !acc;
              current := None
          | Some t, _ ->
              (match parse_op_line words with
              | Some op -> current := Some { t with aops = op :: t.aops }
              | None -> ())
          | None, _ -> ())
      lines;
    let out = List.rev !acc in
    if out = [] then default_reconfig_ast else out

let find_group groups name =
  List.assoc_opt name groups

let validate_system_spec ss =
  let issues = ref [] in
  let comp_names = List.map fst ss.s_components in
  let exists n = List.mem n comp_names in
  let add_issue s = issues := s :: !issues in
  List.iter
    (fun (g, members) ->
      if members = [] then add_issue ("group " ^ g ^ " is empty");
      List.iter (fun m -> if not (exists m) then add_issue ("group " ^ g ^ " references unknown component " ^ m)) members)
    ss.s_groups;
  List.iter
    (fun (c, p) ->
      if not (exists c) then add_issue ("depends uses unknown child " ^ c);
      if not (exists p) then add_issue ("depends uses unknown parent " ^ p))
    ss.s_deps;
  List.iter
    (fun (a, b) ->
      if not (exists a) then add_issue ("bind uses unknown component " ^ a);
      if not (exists b) then add_issue ("bind uses unknown component " ^ b))
    ss.s_bindings;
  let rec has_cycle node visiting visited deps =
    if List.mem node visiting then true
    else if List.mem node visited then false
    else
      let parents = List.filter_map (fun (c, p) -> if c = node then Some p else None) deps in
      List.exists (fun p -> has_cycle p (node :: visiting) (node :: visited) deps) parents
  in
  List.iter (fun (c, _) -> if has_cycle c [] [] ss.s_deps then add_issue ("dependency cycle through " ^ c)) ss.s_components;
  List.sort_uniq String.compare !issues

let validate_reconfig_ast ast =
  let issues = ref [] in
  let names = Hashtbl.create 17 in
  List.iter
    (fun t ->
      if Hashtbl.mem names t.aname then issues := ("duplicate transaction " ^ t.aname) :: !issues
      else Hashtbl.add names t.aname ();
      if t.aops = [] then issues := ("transaction " ^ t.aname ^ " has no operation") :: !issues;
      List.iter
        (function
          | A_add (_, Some w)
          | A_remove (_, Some w)
          | A_start (_, Some w)
          | A_stop (_, Some w)
          | A_update (_, Some w) ->
              if w <= 0 then issues := ("transaction " ^ t.aname ^ " has non-positive within") :: !issues
          | A_bind (_, _, Some w)
          | A_unbind (_, _, Some w) ->
              if w <= 0 then issues := ("transaction " ^ t.aname ^ " has non-positive within") :: !issues
          | _ -> ())
        t.aops)
    ast;
  List.sort_uniq String.compare !issues

let resolve_name_opt id_of_name name =
  List.assoc_opt name id_of_name

let resolve_target_opt id_of_name groups target =
  if String.length target > 1 && target.[0] = '@' then
    let g = String.sub target 1 (String.length target - 1) in
    Option.map (List.map (fun n -> resolve_name_opt id_of_name n)) (find_group groups g)
  else Some [ resolve_name_opt id_of_name target ]

let resolve_ids_or_issue ids label issues =
  if List.exists Option.is_none ids then (
    issues := (label ^ " references unknown component") :: !issues;
    None)
  else Some (List.map Option.get ids)

let resolve_op id_of_name groups issues opname = function
  | A_add (n, w) -> Some [ (Add_component n, w) ]
  | A_remove (n, w) ->
      (match resolve_target_opt id_of_name groups n with
      | None ->
          issues := (opname ^ " unknown group in remove " ^ n) :: !issues;
          None
      | Some ids -> Option.map (List.map (fun id -> (Remove_component id, w))) (resolve_ids_or_issue ids opname issues))
  | A_start (n, w) ->
      (match resolve_target_opt id_of_name groups n with
      | None ->
          issues := (opname ^ " unknown group in start " ^ n) :: !issues;
          None
      | Some ids -> Option.map (List.map (fun id -> (Start id, w))) (resolve_ids_or_issue ids opname issues))
  | A_stop (n, w) ->
      (match resolve_target_opt id_of_name groups n with
      | None ->
          issues := (opname ^ " unknown group in stop " ^ n) :: !issues;
          None
      | Some ids -> Option.map (List.map (fun id -> (Stop id, w))) (resolve_ids_or_issue ids opname issues))
  | A_update (n, w) ->
      (match resolve_target_opt id_of_name groups n with
      | None ->
          issues := (opname ^ " unknown group in update " ^ n) :: !issues;
          None
      | Some ids -> Option.map (List.map (fun id -> (Update id, w))) (resolve_ids_or_issue ids opname issues))
  | A_bind (a, b, w) ->
      (match resolve_target_opt id_of_name groups a, resolve_target_opt id_of_name groups b with
      | Some as_, Some bs ->
          (match resolve_ids_or_issue as_ opname issues, resolve_ids_or_issue bs opname issues with
          | Some axs, Some bxs -> Some (List.concat_map (fun x -> List.map (fun y -> (Bind (x, y), w)) bxs) axs)
          | _ -> None)
      | _ ->
          issues := (opname ^ " unknown group in bind") :: !issues;
          None)
  | A_unbind (a, b, w) ->
      (match resolve_target_opt id_of_name groups a, resolve_target_opt id_of_name groups b with
      | Some as_, Some bs ->
          (match resolve_ids_or_issue as_ opname issues, resolve_ids_or_issue bs opname issues with
          | Some axs, Some bxs -> Some (List.concat_map (fun x -> List.map (fun y -> (Unbind (x, y), w)) bxs) axs)
          | _ -> None)
      | _ ->
          issues := (opname ^ " unknown group in unbind") :: !issues;
          None)

let build_from_specs seed system_file reconfig_file =
  let ss = load_system_spec system_file in
  let issues = ref (validate_system_spec ss) in
  let comps = ss.s_components |> List.mapi (fun id (name, st) -> { id; name; state = st }) |> Array.of_list in
  let id_of_name = Array.to_list comps |> List.map (fun c -> (c.name, c.id)) in
  let resolve_pair_safe tag (a, b) =
    match resolve_name_opt id_of_name a, resolve_name_opt id_of_name b with
    | Some x, Some y -> Some (x, y)
    | _ ->
        issues := (tag ^ " unresolved: " ^ a ^ " " ^ b) :: !issues;
        None
  in
  let deps = ss.s_deps |> List.filter_map (resolve_pair_safe "depends") in
  let bindings = ss.s_bindings |> List.filter_map (resolve_pair_safe "bind") in
  let ast = load_reconfig_ast reconfig_file in
  issues := validate_reconfig_ast ast @ !issues;
  let templates =
    ast
    |> List.filter_map (fun t ->
           let resolved_ops =
             t.aops
             |> List.filter_map (resolve_op id_of_name ss.s_groups issues t.aname)
             |> List.concat
           in
           if resolved_ops = [] then (
             issues := ("transaction " ^ t.aname ^ " dropped: no valid operation") :: !issues;
             None)
           else Some { tname = t.aname; tops = resolved_ops; tdeadline = t.adeadline })
  in
  let spec_issues = List.sort_uniq String.compare !issues in
  {
    step = 0;
    seed;
    comps;
    deps;
    bindings;
    link_ttl = Array.make (max 1 (List.length deps)) 100;
    partition = false;
    loss_rate = 0.12;
    policy = ss.s_policy;
    scenario = ss.s_scenario;
    mode = Normal;
    txs = [];
    templates;
    next_template_idx = 0;
    next_tx = 1;
    commits = 0;
    aborts = 0;
    rollbacks = 0;
    violations = 0;
    invariant_ok = true;
    last_event = (if spec_issues = [] then "Boot" else "Boot with spec warnings");
    event_log = [];
    auto_clock = 0;
    spec_issues;
    system_file;
    reconfig_file;
  }

let pseudo_loss seed step id =
  let v = (seed * 17 + step * 37 + id * 97 + 11) mod 1000 in
  float_of_int v /. 1000.0

let find_comp comps id = Array.find_opt (fun c -> c.id = id) comps
let set_comp_state comps id st = Array.map (fun c -> if c.id = id then { c with state = st } else c) comps
let has_comp comps id = Array.exists (fun c -> c.id = id) comps
let has_binding bindings a b = List.exists (fun (x, y) -> x = a && y = b) bindings
let add_binding bindings a b = if has_binding bindings a b then bindings else (a, b) :: bindings
let remove_binding bindings a b = List.filter (fun (x, y) -> not (x = a && y = b)) bindings
let deps_for_child deps child = List.filter_map (fun (c, p) -> if c = child then Some p else None) deps
let dependents deps parent = List.filter_map (fun (c, p) -> if p = parent then Some c else None) deps

let comp_resources op =
  match op with
  | Add_component _ -> [ -1 ]
  | Remove_component i | Start i | Stop i | Update i -> [ i ]
  | Bind (a, b) | Unbind (a, b) -> [ a; b ]

let tx_resources tx = Array.to_list tx.steps |> List.concat_map comp_resources |> List.sort_uniq compare
let conflicts tx_a tx_b = List.exists (fun x -> List.mem x (tx_resources tx_b)) (tx_resources tx_a)

let take n xs =
  let rec loop i acc = function
    | [] -> List.rev acc
    | _ when i <= 0 -> List.rev acc
    | x :: rest -> loop (i - 1) (x :: acc) rest
  in
  loop n [] xs

let log_event m kind detail =
  let e = { step = m.step; kind; detail } in
  { m with event_log = take 220 (e :: m.event_log); last_event = detail }

let scenario_loss scenario base =
  match scenario with Baseline -> base | Lossy -> min 0.9 (base +. 0.22) | Partitioned -> min 0.9 (base +. 0.15)

let scenario_partition step scenario old =
  match scenario with
  | Baseline -> old
  | Lossy -> if step mod 1200 = 0 then not old else old
  | Partitioned ->
      if step mod 420 = 0 then true else if old && step mod 420 = 120 then false else old

let update_links m =
  let loss = scenario_loss m.scenario m.loss_rate in
  Array.mapi
    (fun i ttl ->
      let lost = m.partition || pseudo_loss m.seed m.step (40 + i) < loss in
      if lost then max 0 (ttl - 8) else min 100 (ttl + 3))
    m.link_ttl

let op_precondition m op =
  let loss = scenario_loss m.scenario m.loss_rate in
  match op with
  | Add_component _ -> true, ""
  | Remove_component id ->
      if not (has_comp m.comps id) then false, "missing component"
      else if
        List.exists
          (fun d -> match find_comp m.comps d with Some c -> c.state = Running | None -> false)
          (dependents m.deps id)
      then false, "running dependent"
      else true, ""
  | Bind (a, b) ->
      if not (has_comp m.comps a && has_comp m.comps b) then false, "missing endpoint"
      else if has_binding m.bindings a b then false, "already bound"
      else true, ""
  | Unbind (a, b) ->
      if not (has_binding m.bindings a b) then false, "not bound" else true, ""
  | Start id ->
      (match find_comp m.comps id with
      | None -> false, "missing component"
      | Some c when c.state = Running -> false, "already running"
      | Some _ ->
          let parents = deps_for_child m.deps id in
          let ok_parent = List.for_all (fun p -> match find_comp m.comps p with Some cp -> cp.state = Running | None -> false) parents in
          let ok_bind = List.for_all (fun p -> has_binding m.bindings id p) parents in
          if not ok_parent then false, "dependency stopped"
          else if not ok_bind then false, "missing binding"
          else true, "")
  | Stop id ->
      if
        List.exists
          (fun d -> match find_comp m.comps d with Some c -> c.state = Running | None -> false)
          (dependents m.deps id)
      then false, "running dependent"
      else true, ""
  | Update id ->
      (match find_comp m.comps id with
      | Some c when c.state = Running ->
          let link = id mod Array.length m.link_ttl in
          if m.link_ttl.(link) < 12 then false, "link timeout"
          else if pseudo_loss m.seed m.step id < loss then false, "network drop"
          else true, ""
      | _ -> false, "component not running")

let apply_op m op =
  match op with
  | Add_component name ->
      let newid = Array.fold_left (fun a c -> max a (c.id + 1)) 0 m.comps in
      let c = { id = newid; name; state = Stopped } in
      ({ m with comps = Array.append m.comps [| c |] }, Some (Remove_component newid))
  | Remove_component id ->
      let comps = Array.of_list (List.filter (fun c -> c.id <> id) (Array.to_list m.comps)) in
      ( { m with
          comps;
          deps = List.filter (fun (a, b) -> a <> id && b <> id) m.deps;
          bindings = List.filter (fun (a, b) -> a <> id && b <> id) m.bindings
        },
        None )
  | Bind (a, b) -> ({ m with bindings = add_binding m.bindings a b }, Some (Unbind (a, b)))
  | Unbind (a, b) -> ({ m with bindings = remove_binding m.bindings a b }, Some (Bind (a, b)))
  | Start id -> ({ m with comps = set_comp_state m.comps id Running }, Some (Stop id))
  | Stop id -> ({ m with comps = set_comp_state m.comps id Stopped }, Some (Start id))
  | Update id -> ({ m with comps = set_comp_state m.comps id Updating }, Some (Start id))

let finish_update m =
  { m with comps = Array.map (fun c -> if c.state = Updating then { c with state = Running } else c) m.comps }

let tx_is_active tx = match tx.status with Running_tx | Rolling_back _ -> true | _ -> false

let schedule_waiting_txs txs policy =
  let running = List.filter tx_is_active txs in
  let slots = max 0 (policy.max_parallel - List.length running) in
  if slots = 0 then txs
  else
    let rec select acc chosen left = function
      | [] -> List.rev acc
      | tx :: rest ->
          let waiting = tx.status = Waiting in
          let no_conflict = List.for_all (fun c -> not (conflicts tx c)) chosen in
          if waiting && no_conflict && left > 0 then
            let tx' = { tx with status = Running_tx } in
            select (tx' :: acc) (tx' :: chosen) (left - 1) rest
          else select (tx :: acc) chosen left rest
    in
    select [] running slots txs

let update_step_status arr idx v = Array.mapi (fun i x -> if i = idx then v else x) arr

let fail_tx m tx reason =
  if m.policy.rollback_on_failure then { tx with status = Rolling_back 0 }
  else { tx with status = Aborted_tx reason }

let progress_tx m tx =
  match tx.status with
  | Waiting | Committed_tx | Rolled_back | Aborted_tx _ -> m, tx, None
  | Rolling_back i ->
      if i >= List.length tx.compensation then
        let tx' = { tx with status = Rolled_back } in
        (m, tx', Some "rollback done")
      else
        let op = List.nth tx.compensation i in
        let m', _ = apply_op m op in
        let tx' = { tx with status = Rolling_back (i + 1) } in
        (m', tx', Some ("rollback " ^ op_name op))
  | Running_tx ->
      if m.step - tx.created_at > tx.deadline then
        let tx' = fail_tx m tx "tx deadline" in
        (m, tx', Some "deadline exceeded")
      else if tx.cursor >= Array.length tx.steps then
        let tx' = { tx with status = Committed_tx } in
        (m, tx', Some "transaction committed")
      else
        let op = tx.steps.(tx.cursor) in
        let within_ok =
          match tx.step_limits.(tx.cursor) with
          | None -> true
          | Some lim -> m.step - tx.created_at <= lim
        in
        if not within_ok then
          let status = update_step_status tx.step_status tx.cursor (Failed "op deadline") in
          let tx' = fail_tx m { tx with step_status = status } "op deadline" in
          (m, tx', Some "step failed: op deadline")
        else
          let ok, reason = op_precondition m op in
          if not ok then
            let status = update_step_status tx.step_status tx.cursor (Failed reason) in
            let tx' = fail_tx m { tx with step_status = status } reason in
            (m, tx', Some ("step failed: " ^ reason))
          else
            let status = update_step_status tx.step_status tx.cursor Done in
            let m', comp = apply_op m op in
            let compensation = match comp with Some c -> c :: tx.compensation | None -> tx.compensation in
            let tx' = { tx with cursor = tx.cursor + 1; step_status = status; compensation } in
            (m', tx', Some ("step done: " ^ op_name op))

let finalize_tx_metrics m old_tx new_tx =
  match old_tx.status, new_tx.status with
  | Running_tx, Committed_tx -> { m with commits = m.commits + 1 }
  | Running_tx, Rolling_back _ -> { m with aborts = m.aborts + 1; rollbacks = m.rollbacks + 1 }
  | Running_tx, Aborted_tx _ -> { m with aborts = m.aborts + 1 }
  | _, _ -> m

let evaluate_invariants m =
  let dep_ok =
    List.for_all
      (fun (child, parent) ->
        match find_comp m.comps child, find_comp m.comps parent with
        | Some c, Some p ->
            if c.state = Running then p.state = Running && has_binding m.bindings child parent else true
        | _ -> false)
      m.deps
  in
  let links_ok = Array.for_all (fun ttl -> ttl >= 3) m.link_ttl in
  dep_ok && links_ok

let queue_tx m label ops deadline =
  let steps = Array.of_list (List.map fst ops) in
  let limits = Array.of_list (List.map snd ops) in
  let tx =
    {
      id = m.next_tx;
      label;
      steps;
      step_limits = limits;
      step_status = Array.make (Array.length steps) (Pending 0);
      cursor = 0;
      status = Waiting;
      compensation = [];
      created_at = m.step;
      deadline = (match deadline with Some d -> d | None -> m.policy.tx_deadline);
    }
  in
  let m = { m with txs = m.txs @ [ tx ]; next_tx = m.next_tx + 1 } in
  log_event m "queue" (Printf.sprintf "tx#%d %s" tx.id label)

let queue_next_template m =
  match m.templates with
  | [] -> log_event m "queue" "no template loaded"
  | ts ->
      let idx = m.next_template_idx mod List.length ts in
      let t = List.nth ts idx in
      let m = queue_tx m t.tname t.tops t.tdeadline in
      { m with next_template_idx = idx + 1 }

let auto_plan m =
  if m.auto_clock mod m.policy.auto_period <> 0 then m else queue_next_template m

let tick_model m =
  let step = m.step + 1 in
  let partition = scenario_partition step m.scenario m.partition in
  let m = { m with step; partition; auto_clock = m.auto_clock + 1; link_ttl = update_links { m with step; partition } } in
  let m = finish_update m in
  let m = auto_plan m in
  let txs = schedule_waiting_txs m.txs m.policy in
  let rec fold_tx acc_m acc_txs = function
    | [] -> acc_m, List.rev acc_txs
    | tx :: rest ->
        let m1, tx1, evt = progress_tx acc_m tx in
        let m2 = finalize_tx_metrics m1 tx tx1 in
        let m3 =
          match evt with
          | None -> m2
          | Some d -> log_event m2 "tx-step" (Printf.sprintf "tx#%d %s" tx.id d)
        in
        fold_tx m3 (tx1 :: acc_txs) rest
  in
  let m, txs = fold_tx m [] txs in
  let inv = evaluate_invariants m in
  let m = { m with txs; invariant_ok = inv } in
  if inv then m else { m with violations = m.violations + 1 }

let draw_bar x y w h ratio color label =
  let r = if ratio < 0.0 then 0.0 else if ratio > 1.0 then 1.0 else ratio in
  draw_rectangle x y w h (Color.create 20 33 46 220);
  draw_rectangle (x + 2) (y + 2) (int_of_float (float_of_int (w - 4) *. r)) (h - 4) color;
  draw_rectangle_lines x y w h (Color.create 138 178 206 255);
  draw_text label x (y - 16) 16 (Color.create 194 221 244 255)

let draw_model m =
  let comp_pos = [| (170, 220); (410, 150); (650, 220); (410, 360); (890, 220); (890, 360) |] in
  begin_drawing ();
  clear_background (Color.create 14 24 38 255);
  let panel = Tempo_game.Hud.panel ~rect:{ Tempo_game.Ui.x = 14.0; y = 10.0; w = 1092.0; h = 124.0 } ~title:"" in
  Tempo_game_raylib.Hud.draw_panel panel;
  draw_text "Reactive Reconfiguration Engine V6 (Declarative Concerto-style)" 22 18 24 (Color.create 220 238 252 255);
  draw_text
    (Printf.sprintf "step=%d mode=%s policy=%s [par=%d ddl=%d auto=%d rb=%b] scenario=%s partition=%b loss=%.0f%% inv=%s"
       m.step (mode_name m.mode)
       m.policy.pname m.policy.max_parallel m.policy.tx_deadline m.policy.auto_period m.policy.rollback_on_failure
       (scenario_name m.scenario) m.partition (scenario_loss m.scenario m.loss_rate *. 100.0)
       (if m.invariant_ok then "OK" else "BROKEN"))
    22 50 18 (if m.invariant_ok then Color.create 170 224 179 255 else Color.create 244 134 106 255);
  draw_text
    (Printf.sprintf "commits=%d aborts=%d rollbacks=%d violations=%d templates=%d active_txs=%d spec_issues=%d"
       m.commits m.aborts m.rollbacks m.violations (List.length m.templates) (List.length (List.filter tx_is_active m.txs))
       (List.length m.spec_issues))
    22 76 18 (Color.create 187 217 240 255);
  draw_text m.last_event 22 102 18 (Color.create 201 224 245 255);

  List.iteri
    (fun i (c, p) ->
      if c < Array.length comp_pos && p < Array.length comp_pos then (
        let x1, y1 = comp_pos.(c) in
        let x2, y2 = comp_pos.(p) in
        let ttl = m.link_ttl.(i mod Array.length m.link_ttl) in
        let col =
          if ttl < 12 then Color.create 227 107 90 240
          else if ttl < 40 then Color.create 232 184 95 235
          else Color.create 110 152 196 220
        in
        draw_line_ex (Vector2.create (float_of_int x1) (float_of_int y1)) (Vector2.create (float_of_int x2) (float_of_int y2)) 4.0 col;
        draw_text (Printf.sprintf "L%d:%d" i ttl) ((x1 + x2) / 2 - 18) ((y1 + y2) / 2 - 18) 14 (Color.create 218 236 250 255)))
    m.deps;

  Array.iter
    (fun c ->
      let x, y = if c.id < Array.length comp_pos then comp_pos.(c.id) else (980, 500) in
      let col =
        match c.state with
        | Running -> Color.create 70 178 110 255
        | Stopped -> Color.create 178 86 86 255
        | Updating -> Color.create 229 172 88 255
      in
      draw_circle x y 44.0 (Color.create 0 0 0 90);
      draw_circle x y 40.0 col;
      draw_circle_lines x y 40.0 (Color.create 222 237 249 255);
      draw_text c.name (x - 34) (y - 10) 17 Color.raywhite;
      draw_text (state_name c.state) (x - 24) (y + 14) 14 (Color.create 25 39 52 255))
    m.comps;

  let active_txs = List.filter (fun t -> t.status <> Committed_tx && t.status <> Rolled_back) m.txs |> take 4 in
  List.iteri
    (fun i tx ->
      let y = 460 + (i * 56) in
      draw_rectangle 18 y 760 48 (Color.create 18 35 52 220);
      draw_rectangle_lines 18 y 760 48 (Color.create 129 172 204 220);
      let st =
        match tx.status with
        | Waiting -> "waiting"
        | Running_tx -> "running"
        | Committed_tx -> "committed"
        | Aborted_tx r -> "aborted:" ^ r
        | Rolling_back _ -> "rollback"
        | Rolled_back -> "rolled_back"
      in
      draw_text (Printf.sprintf "tx#%d %s [%s]" tx.id tx.label st) 30 (y + 6) 16 (Color.create 206 228 246 255);
      if tx.cursor < Array.length tx.steps then (
        let step_label =
          match tx.step_limits.(tx.cursor) with
          | None -> op_name tx.steps.(tx.cursor)
          | Some w -> Printf.sprintf "%s (within %d)" (op_name tx.steps.(tx.cursor)) w
        in
        draw_text (Printf.sprintf "next: %s" step_label) 30 (y + 24) 14 (Color.create 170 210 238 255);
        draw_text (step_status_text tx.step_status.(tx.cursor)) 470 (y + 24) 14 (Color.create 188 218 240 255)))
    active_txs;

  let avg_link =
    let sum = Array.fold_left ( + ) 0 m.link_ttl in
    float_of_int sum /. float_of_int (Array.length m.link_ttl * 100)
  in
  draw_bar 812 474 280 22 avg_link (Color.create 98 197 223 255) "Link health";
  draw_bar 812 522 280 22 (float_of_int m.commits /. float_of_int (max 1 (m.commits + m.aborts))) (Color.create 120 210 145 255) "Commit ratio";
  draw_bar 812 570 280 22 (float_of_int m.violations /. float_of_int (max 1 (m.step / 40 + 1))) (Color.create 238 132 104 255) "Violation pressure";
  (match m.spec_issues with
  | issue :: _ ->
      draw_text ("spec warning: " ^ issue) 18 636 16 (Color.create 243 180 116 255)
  | [] -> ());
  draw_text "T: queue template | R: reload specs | P: partition | L/K: loss +/- | Y: policy preset | U: scenario | ESC quit"
    18 666 16 (Color.create 188 217 239 255);
  end_drawing ()

let parse_args () =
  let headless = ref false in
  let steps = ref 2400 in
  let seed = ref 7 in
  let scenario = ref None in
  let policy = ref None in
  let trace_dir = ref "applications/reconfiguration/reactive-reconfiguration-engine/traces" in
  let system_file = ref default_system_file in
  let reconfig_file = ref default_reconfig_file in
  Array.iteri
    (fun i a ->
      if a = "--headless" then headless := true
      else if a = "--steps" && i + 1 < Array.length Sys.argv then steps := int_of_string Sys.argv.(i + 1)
      else if a = "--seed" && i + 1 < Array.length Sys.argv then seed := int_of_string Sys.argv.(i + 1)
      else if a = "--scenario" && i + 1 < Array.length Sys.argv then scenario := Some (parse_scenario (String.lowercase_ascii Sys.argv.(i + 1)))
      else if a = "--policy" && i + 1 < Array.length Sys.argv then policy := Some (profile_from_preset (String.lowercase_ascii Sys.argv.(i + 1)))
      else if a = "--trace-dir" && i + 1 < Array.length Sys.argv then trace_dir := Sys.argv.(i + 1)
      else if a = "--system-file" && i + 1 < Array.length Sys.argv then system_file := Sys.argv.(i + 1)
      else if a = "--reconfig-file" && i + 1 < Array.length Sys.argv then reconfig_file := Sys.argv.(i + 1))
    Sys.argv;
  (!headless, !steps, !seed, !scenario, !policy, !trace_dir, !system_file, !reconfig_file)

let next_policy_profile p =
  match String.lowercase_ascii p.pname with
  | "conservative" -> profile_balanced
  | "balanced" -> profile_aggressive
  | "aggressive" -> profile_conservative
  | _ -> profile_balanced

let next_scenario = function Baseline -> Lossy | Lossy -> Partitioned | Partitioned -> Baseline

let run_headless steps seed scenario policy trace_dir system_file reconfig_file =
  let _ = Sys.command ("mkdir -p " ^ Filename.quote trace_dir) in
  let m0 = build_from_specs seed system_file reconfig_file in
  let m0 =
    { m0 with
      scenario = (match scenario with Some s -> s | None -> m0.scenario);
      policy = (match policy with Some p -> p | None -> m0.policy)
    }
  in
  let rec loop i m = if i <= 0 then m else loop (i - 1) (tick_model m) in
  let m = loop steps m0 in
  let stamp = Printf.sprintf "s%d_%s_%s" seed m.policy.pname (scenario_name m.scenario) in
  let csv_path = Filename.concat trace_dir (stamp ^ ".csv") in
  let json_path = Filename.concat trace_dir (stamp ^ ".json") in
  let oc = open_out csv_path in
  output_string oc "step,kind,detail\n";
  List.rev m.event_log
  |> List.iter (fun (e : event) -> Printf.fprintf oc "%d,%s,%s\n" e.step e.kind (String.map (fun c -> if c = ',' then ';' else c) e.detail));
  close_out oc;
  let oj = open_out json_path in
  Printf.fprintf oj "{\n";
  Printf.fprintf oj "  \"seed\": %d,\n" seed;
  Printf.fprintf oj "  \"policy\": \"%s\",\n" m.policy.pname;
  Printf.fprintf oj "  \"max_parallel\": %d,\n" m.policy.max_parallel;
  Printf.fprintf oj "  \"tx_deadline\": %d,\n" m.policy.tx_deadline;
  Printf.fprintf oj "  \"auto_period\": %d,\n" m.policy.auto_period;
  Printf.fprintf oj "  \"rollback_on_failure\": %b,\n" m.policy.rollback_on_failure;
  Printf.fprintf oj "  \"scenario\": \"%s\",\n" (scenario_name m.scenario);
  Printf.fprintf oj "  \"steps\": %d,\n" m.step;
  Printf.fprintf oj "  \"commits\": %d,\n" m.commits;
  Printf.fprintf oj "  \"aborts\": %d,\n" m.aborts;
  Printf.fprintf oj "  \"rollbacks\": %d,\n" m.rollbacks;
  Printf.fprintf oj "  \"violations\": %d,\n" m.violations;
  Printf.fprintf oj "  \"spec_issues\": %d,\n" (List.length m.spec_issues);
  Printf.fprintf oj "  \"invariant_ok\": %b\n" m.invariant_ok;
  Printf.fprintf oj "}\n";
  close_out oj;
  if m.spec_issues <> [] then (
    Printf.printf "Spec issues (%d):\n" (List.length m.spec_issues);
    List.iter (fun s -> Printf.printf "  - %s\n" s) (take 10 m.spec_issues));
  Printf.printf "Headless completed: commits=%d aborts=%d rollbacks=%d violations=%d\n" m.commits m.aborts m.rollbacks m.violations;
  Printf.printf "Traces: %s\nSummary: %s\n%!" csv_path json_path

let update m ev =
  match ev with
  | Boot -> m, App.after_n 1 Tick
  | Tick -> tick_model m, App.after_n 1 Tick
  | Set_mode mode -> log_event { m with mode } "mode" ("enter " ^ mode_name mode), App.none
  | Toggle_partition -> log_event { m with partition = not m.partition } "network" "partition toggled", App.none
  | Loss_up -> log_event { m with loss_rate = min 0.9 (m.loss_rate +. 0.05) } "network" "loss increased", App.none
  | Loss_down -> log_event { m with loss_rate = max 0.0 (m.loss_rate -. 0.05) } "network" "loss decreased", App.none
  | Next_policy ->
      let p = next_policy_profile m.policy in
      log_event { m with policy = p } "policy" ("switch preset to " ^ p.pname), App.none
  | Next_scenario ->
      let s = next_scenario m.scenario in
      log_event { m with scenario = s } "scenario" ("switch to " ^ scenario_name s), App.none
  | Queue_next_template -> queue_next_template m, App.none
  | Reload_specs ->
      let fresh = build_from_specs m.seed m.system_file m.reconfig_file in
      let fresh = { fresh with step = m.step; event_log = m.event_log } in
      let msg =
        if fresh.spec_issues = [] then "specs reloaded"
        else Printf.sprintf "specs reloaded with %d warning(s)" (List.length fresh.spec_issues)
      in
      log_event fresh "spec" msg, App.none

type ui_input = {
  quit : bool;
  t : bool;
  r : bool;
  p : bool;
  l : bool;
  k : bool;
  y : bool;
  u : bool;
}

let read_ui_input () =
  Some
    {
      quit = window_should_close () || is_key_pressed Key.Escape;
      t = is_key_pressed Key.T;
      r = is_key_pressed Key.R;
      p = is_key_pressed Key.P;
      l = is_key_pressed Key.L;
      k = is_key_pressed Key.K;
      y = is_key_pressed Key.Y;
      u = is_key_pressed Key.U;
    }

let run_reactive_loop init =
  let stop = new_signal () in
  let cmd_bus : msg Event_bus.channel = Event_bus.channel () in
  let model_updates : model signal = new_signal () in
  let mode_scene =
    Scene.create
      ~on_enter:(fun m -> Event_bus.publish cmd_bus (Set_mode m))
      ~on_exit:(fun _ -> ())
      ()
  in
  let rec input_proc input_sig =
    let i = await input_sig in
    if i.quit then emit stop ()
    else (
      if i.t then Event_bus.publish cmd_bus Queue_next_template;
      if i.r then Event_bus.publish cmd_bus Reload_specs;
      if i.p then Event_bus.publish cmd_bus Toggle_partition;
      if i.l then Event_bus.publish cmd_bus Loss_up;
      if i.k then Event_bus.publish cmd_bus Loss_down;
      if i.y then Event_bus.publish cmd_bus Next_policy;
      if i.u then Event_bus.publish cmd_bus Next_scenario;
      input_proc input_sig)
  in
  let tick_proc () =
    Game.every_n 1 (fun () -> Event_bus.publish cmd_bus Tick)
  in
  let rec monitor_proc current_mode =
    let m = await model_updates in
    let next_mode =
      if not m.invariant_ok || m.partition || m.spec_issues <> [] then Degraded
      else if m.rollbacks > 0 && (m.step mod 220) < 80 then Recovery
      else Normal
    in
    if next_mode <> current_mode then Scene.request mode_scene next_mode;
    monitor_proc next_mode
  in
  let rec model_proc output_sig m =
    let cmds = Event_bus.await_batch cmd_bus in
    let m' = List.fold_left (fun acc cmd -> fst (update acc cmd)) m cmds in
    emit output_sig m';
    emit model_updates m';
    model_proc output_sig m'
  in
  fun input_sig output_sig ->
    parallel
      [
        (fun () ->
          watch stop (fun () ->
              parallel
                [
                  (fun () ->
                    Event_bus.publish cmd_bus Boot;
                    model_proc output_sig init);
                  (fun () -> tick_proc ());
                  (fun () -> input_proc input_sig);
                  (fun () -> Scene.process mode_scene);
                  (fun () -> monitor_proc init.mode);
                ]));
      ]

let () =
  let headless, steps, seed, scenario, policy, trace_dir, system_file, reconfig_file = parse_args () in
  if headless then run_headless steps seed scenario policy trace_dir system_file reconfig_file
  else (
    init_window width height "Reactive Reconfiguration Engine - Tempo";
    set_target_fps 60;
    Tempo_game_raylib.Audio.init ();
    let init = build_from_specs seed system_file reconfig_file in
    let init =
      {
        init with
        scenario = (match scenario with Some s -> s | None -> init.scenario);
        policy = (match policy with Some p -> p | None -> init.policy);
      }
    in
    Tempo.execute ~instants:2_000_000 ~input:read_ui_input ~output:draw_model
      (run_reactive_loop init);
    Tempo_game_raylib.Audio.shutdown ();
    close_window ())
