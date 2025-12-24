(*---------------------------------------------------------------------------
 * Tempo - synchronous runtime for OCaml
 * Copyright (C) 2025 Frédéric Dabrowski
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *---------------------------------------------------------------------------*)

open Tempo_types

module Tempo_log = struct
  module Backend_logs = Logs

  (* Runtime logging helpers: reporter setup, colorized scopes, formatting utilities,
     and printer functions for runtime data structures. *)

  (* A lightweight record that captures the current instant/step when emitting logs.
     The scheduler rebuilds it at each log call so we only pass immutable data. *)
  type context =
    { instant : int
    ; step : int
    }

  let context ~instant ~step = { instant; step }

  (* --- Tag helpers -------------------------------------------------------- *)
  module Log_tags = struct
    let task_id : int Backend_logs.Tag.def =
      Backend_logs.Tag.def "task" ~doc:"Task identifier" Format.pp_print_int

    let signal_id : int Backend_logs.Tag.def =
      Backend_logs.Tag.def "signal" ~doc:"Signal identifier" Format.pp_print_int
  end

  let add_opt_tag def value tags =
    match value with
    | None -> tags
    | Some v -> Backend_logs.Tag.add def v tags

  (* --- Color selection ---------------------------------------------------- *)
  let ansi_reset = "\027[0m"

  let use_color =
    match Sys.getenv_opt "RML_LOG_COLOR" with
    | Some ("0" | "false" | "no" | "off" | "FALSE" | "NO" | "OFF") -> false
    | Some _ -> true
    | None -> true

  (* Guard tracing can be toggled dynamically via env var. *)
  let trace_guards =
    match Sys.getenv_opt "RML_TRACE_GUARDS" with
    | Some ("1" | "true" | "yes" | "on" | "TRUE" | "YES" | "ON") -> true
    | _ -> false

let color_of_scope scope =
  if String.starts_with ~prefix:"instant" scope then "\027[35m"
  else if String.starts_with ~prefix:"step" scope then "\027[33m"
  else if String.starts_with ~prefix:"tasks" scope then "\027[32m"
  else if String.starts_with ~prefix:"signals" scope then "\027[36m"
  else if String.starts_with ~prefix:"queues" scope then "\027[34m"
  else if String.starts_with ~prefix:"run" scope then "\027[36m"
  else "\027[37m"

let colorize scope text =
  if use_color then color_of_scope scope ^ text ^ ansi_reset else text

(* --- Logging front-end -------------------------------------------------- *)
let log ?(level = Backend_logs.Debug) ?task ?signal ctx scope fmt =
  Format.kasprintf
    (fun msg ->
      let tags =
        Backend_logs.Tag.empty
        |> add_opt_tag Log_tags.task_id task
        |> add_opt_tag Log_tags.signal_id signal
      in
      let formatted =
        Format.asprintf "[%s][I%03d|S%03d] %s" scope ctx.instant ctx.step msg
      in
      let colored = colorize scope formatted in
      Backend_logs.msg level (fun m -> m ~tags "%s" colored))
    fmt

let log_banner ctx scope label =
  let line = String.make 18 '-' in
  log ~level:Backend_logs.Debug ctx scope "%s %s %s" line label line

let log_info ctx scope label =
  log ~level:Backend_logs.Info ctx scope "%s" label

(* --- Printers for runtime data structures ------------------------------- *)
let snapshot_queue q =
  if Queue.is_empty q then [] else List.of_seq (Queue.to_seq q)

let default_sep fmt () = Format.fprintf fmt "; "

let pp_task ?(brief=false) fmt t =
  let queued_msg = if t.queued then "queued" else "not queued" in
  let blocked_msg = if t.blocked then "blocked" else "not blocked" in
  if brief then
    Format.fprintf fmt "#%d (%s,%s)" t.t_id queued_msg blocked_msg
  else
    let guards = List.length t.guards and kills = List.length t.kills in
    Format.fprintf fmt "[task %d | %s, %s, guards=%d, kills=%d]"
      t.t_id queued_msg blocked_msg guards kills

let pp_task_id fmt t = Format.fprintf fmt "%d" t.t_id

let pp_task_id_list ?(pp_sep=default_sep) fmt ts =
  Format.pp_print_list ~pp_sep pp_task_id fmt ts

let pp_task_id_list_default = pp_task_id_list ~pp_sep:default_sep

let pp_task_list ?(pp_sep=default_sep) ?(brief=false) fmt ls =
  let printer = pp_task ~brief in
  Format.pp_print_list ~pp_sep printer fmt ls

let pp_task_list_brief = pp_task_list ~pp_sep:default_sep ~brief:true
let pp_task_list_full = pp_task_list ~pp_sep:default_sep ~brief:false

let pp_limited_list ?(pp_sep=default_sep) ?(max_items=None) printer fmt lst =
  let rec take acc n rest =
    match max_items, rest with
    | Some limit, _ when n >= limit ->
        List.rev acc, true
    | _, [] -> List.rev acc, false
    | _, x :: xs -> take (x :: acc) (n + 1) xs
  in
  let items, truncated = take [] 0 lst in
  Format.pp_print_list ~pp_sep printer fmt items;
  if truncated then Format.pp_print_string fmt "; ..."

let pp_signal ?(brief=false) fmt s =
  let present_msg = if s.present then "present" else "absent" in
  if brief then
    Format.fprintf fmt "[sig %d %s]" s.s_id present_msg
  else
    Format.fprintf fmt "[sig %d | %s, guards=[%a]]"
      s.s_id present_msg pp_task_list_brief s.guard_waiters

let pp_any_signal ?brief fmt (Any s) =
  pp_signal ?brief fmt s

let pp_any_signal_list ?brief =
  Format.pp_print_list (pp_any_signal ?brief)

let pp_any_signal_list_full = pp_any_signal_list ~brief:false

let pp_any_guard ?(brief=true) fmt (Any s) =
  if brief then Format.fprintf fmt "[sig %d]" s.s_id
  else
    let present_msg = if s.present then "present" else "absent" in
    Format.fprintf fmt "[sig %d | %s]" s.s_id present_msg

let pp_any_guard_list ?brief =
  Format.pp_print_list (pp_any_guard ?brief)

let log_queue_state ctx scope current blocked paused =
  let pp_ids = pp_task_id_list_default in
  log ~level:Backend_logs.Debug ctx scope "counts current=%d blocked=%d paused=%d"
    (List.length current) (List.length blocked) (List.length paused);
  log ctx scope "current=[%a] blocked=[%a] paused=[%a]"
    pp_ids current pp_ids blocked pp_ids paused

(* Only log guard-specific messages when tracing is enabled. *)
let log_guard ?task ?signal ctx fmt =
  if trace_guards then log ?task ?signal ctx "guards" fmt
  else Format.ifprintf Format.std_formatter fmt

let pp_span fmt span =
  let ms = Mtime.Span.to_float_ns span /. 1e6 in
  Format.fprintf fmt "%.3fms" ms

(* --- Duration metrics --------------------------------------------------- *)
module Scope_metrics = struct
  type data =
    { mutable count : int
    ; mutable total : Mtime.span
    ; mutable max   : Mtime.span
    }

  let table : (string, data) Hashtbl.t = Hashtbl.create 16

  let ensure scope =
    match Hashtbl.find_opt table scope with
    | Some data -> data
    | None ->
        let data =
          { count = 0; total = Mtime.Span.zero; max = Mtime.Span.zero }
        in
        Hashtbl.add table scope data;
        data

  let record scope span =
    let data = ensure scope in
    data.count <- data.count + 1;
    data.total <- Mtime.Span.add data.total span;
    if Mtime.Span.compare span data.max > 0 then data.max <- span

  let iter f = Hashtbl.iter (fun scope data -> f scope data) table
end

let record_duration scope span =
  Scope_metrics.record scope span

let log_duration_summary () =
  Scope_metrics.iter (fun scope data ->
      let avg =
        if data.count = 0 then Mtime.Span.zero
        else
          let avg_ns =
            Mtime.Span.to_float_ns data.total /. float data.count
          in
          match Mtime.Span.of_float_ns avg_ns with
          | Some span -> span
          | None -> Mtime.Span.zero
      in
      log
        (context ~instant:0 ~step:0)
        "metrics"
        "[%s] count=%d total=%a avg=%a max=%a"
        scope data.count pp_span data.total pp_span avg pp_span data.max)

(* --- Log-level selection / reporter init -------------------------------- *)
type log_level =
  | Quiet
  | Error
  | Warning
  | Info
  | Debug

let log_level_to_logs = function
  | Quiet -> None
  | Error -> Some Backend_logs.Error
  | Warning -> Some Backend_logs.Warning
  | Info -> Some Backend_logs.Info
  | Debug -> Some Backend_logs.Debug

let log_level_of_string s =
  match String.lowercase_ascii s with
  | "debug" -> Some Debug
  | "info" -> Some Info
  | "warn" | "warning" -> Some Warning
  | "error" -> Some Error
  | "quiet" | "none" | "off" -> Some Quiet
  | _ -> None

let set_log_level level =
  Backend_logs.set_level (log_level_to_logs level)

let set_log_level_from_string s =
  match log_level_of_string s with
  | Some level -> set_log_level level
  | None -> invalid_arg (Format.asprintf "Unknown log level '%s'" s)

let log_level_from_cli () =
  let prefix = "--log-level" in
  let prefix_with_equals = prefix ^ "=" in
  let argv = Sys.argv in
  let len = Array.length argv in
  let rec loop idx =
    if idx >= len then None
    else
      let arg = argv.(idx) in
      if String.equal arg prefix then
        if idx + 1 < len then Some argv.(idx + 1)
        else (
          Format.eprintf "--log-level flag provided without a value, ignoring.@.";
          None)
      else if String.starts_with ~prefix:prefix_with_equals arg then
        let value_len = String.length arg - String.length prefix_with_equals in
        Some (String.sub arg (String.length prefix_with_equals) value_len)
      else loop (idx + 1)
  in
  loop 1

end

include Tempo_log

let stamp_tag : Mtime.span Backend_logs.Tag.def =
  Backend_logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.Span.pp

let stamp c = Backend_logs.Tag.(empty |> add stamp_tag (Mtime_clock.count c))

let reporter ppf =
  let report _src level ~over k msgf =
    let k _ = over (); k () in
    let with_stamp h _tags k ppf fmt =
      Format.kfprintf k ppf ("%a@[" ^^ fmt ^^ "@]@.")
        Backend_logs.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  { Backend_logs.report = report }

let init_printer () =
  Backend_logs.set_reporter (reporter (Format.std_formatter))

let init () =
  init_printer ();
  let level =
    match log_level_from_cli () with
    | Some raw -> (
        match log_level_of_string raw with
        | Some lvl -> lvl
        | None ->
            Format.eprintf
              "Unknown value for --log-level (%s), defaulting to quiet.@." raw;
            Quiet)
    | None -> (
        match Sys.getenv_opt "RML_LOG_LEVEL" with
        | None -> Quiet
        | Some raw -> (
            match log_level_of_string raw with
            | Some lvl -> lvl
            | None ->
                Format.eprintf
                  "Unknown value for RML_LOG_LEVEL (%s), defaulting to debug.@."
                  raw;
                Debug))
  in
  set_log_level level
