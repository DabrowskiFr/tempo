(* THIS FILE IS GENERATED. *)
(* /Users/fdabrowski/Documents/Research/Papers/ppdp2026/rml-1.09.07-2021-07-26-ocaml-5/compiler/rmlc -n -1 -sampling -1.0 rml_bench.rml  *)

open Implem_lco_ctrl_tree_record;;
let benchmark = Stdlib.ref "propagation_chains" 
;;
let size = Stdlib.ref 10 
;;
let run_id = Stdlib.ref 1 
;;
let effective_propagation =
      (function | n__val_rml_5  -> Stdlib.max 1 n__val_rml_5 ) 
;;
let effective_broadcast =
      (function | n__val_rml_7  -> Stdlib.max 1 n__val_rml_7 ) 
;;
let effective_fork_depth =
      (function
        | n__val_rml_9  ->
            (let n__val_rml_10 = Stdlib.max 2 n__val_rml_9  in
              Stdlib.max
                1
                (Stdlib.int_of_float
                  (Stdlib.(/.)
                    (Stdlib.log (Stdlib.float_of_int n__val_rml_10))
                    (Stdlib.log 2.))))
        ) 
;;
let effective_guard_depth =
      (function | n__val_rml_12  -> Stdlib.max 1 n__val_rml_12 ) 
;;
let effective_preemption_depth =
      (function | n__val_rml_14  -> Stdlib.max 1 n__val_rml_14 ) 
;;
let effective_multi_rounds =
      (function
        | n__val_rml_16  ->
            (let n__val_rml_17 = Stdlib.max 2 n__val_rml_16  in
              Stdlib.max
                2
                (Stdlib.int_of_float
                  (Stdlib.(/.)
                    (Stdlib.log (Stdlib.float_of_int n__val_rml_17))
                    (Stdlib.log 2.))))
        ) 
;;
let instants_for =
      (function
        | bench__val_rml_19  ->
            (function
              | n__val_rml_20  ->
                  (match bench__val_rml_19 with
                   | "propagation_chains_multi"  ->
                       effective_multi_rounds n__val_rml_20
                   | "broadcast_expansion"  -> 2
                   | "fork_explosion"  -> effective_fork_depth n__val_rml_20
                   | "guarded_cascades_multi"  ->
                       effective_multi_rounds n__val_rml_20
                   | "nested_preemption"  -> 2 | _  -> 1 )
              )
        ) 
;;
let parse_args =
      (function
        | ()  ->
            Arg.parse
              (("--benchmark", (Arg.Set_string benchmark), "Benchmark name")
                ::
                (("--size", (Arg.Set_int size), "Benchmark size parameter")
                  :: (("--run", (Arg.Set_int run_id), "Run index") :: ([]))))
              (function | _  -> () )
              "rml_bench --benchmark <name> --size <n> --run <k>"
        ) 
;;
let link =
      (function
        | s_in__val_rml_24  ->
            (function
              | s_out__val_rml_25  ->
                  ((function
                     | ()  ->
                         Lco_ctrl_tree_record.rml_seq
                           (Lco_ctrl_tree_record.rml_await_immediate'
                             s_in__val_rml_24)
                           (Lco_ctrl_tree_record.rml_emit' s_out__val_rml_25)
                     ):
                    (_) Lco_ctrl_tree_record.process)
              )
        ) 
;;
let rec chain =
          (function
            | n__val_rml_27  ->
                (function
                  | s_in__val_rml_28  ->
                      (function
                        | s_out__val_rml_29  ->
                            ((function
                               | ()  ->
                                   Lco_ctrl_tree_record.rml_if
                                     (function
                                       | ()  -> Stdlib.(<=) n__val_rml_27 (0)
                                       )
                                     (Lco_ctrl_tree_record.rml_compute
                                       (function
                                         | ()  ->
                                             Lco_ctrl_tree_record.rml_expr_emit
                                               s_out__val_rml_29
                                         ))
                                     (Lco_ctrl_tree_record.rml_signal
                                       (function
                                         | mid__sig_30  ->
                                             Lco_ctrl_tree_record.rml_par
                                               (Lco_ctrl_tree_record.rml_run
                                                 (function
                                                   | ()  ->
                                                       link
                                                         s_in__val_rml_28
                                                         mid__sig_30
                                                   ))
                                               (Lco_ctrl_tree_record.rml_run
                                                 (function
                                                   | ()  ->
                                                       chain
                                                         (Stdlib.(-)
                                                           n__val_rml_27 1)
                                                         mid__sig_30
                                                         s_out__val_rml_29
                                                   ))
                                         ))
                               ):
                              (_) Lco_ctrl_tree_record.process)
                        )
                  )
            ) 
;;
let bench_propagation =
      (function
        | n__val_rml_32  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_def
                     (function | ()  -> effective_propagation n__val_rml_32 )
                     (function
                       | n__val_rml_33  ->
                           Lco_ctrl_tree_record.rml_signal
                             (function
                               | start_sig__sig_34  ->
                                   Lco_ctrl_tree_record.rml_signal
                                     (function
                                       | end_sig__sig_35  ->
                                           Lco_ctrl_tree_record.rml_par_n
                                             ((Lco_ctrl_tree_record.rml_seq
                                                (Lco_ctrl_tree_record.rml_run
                                                  (function
                                                    | ()  ->
                                                        chain
                                                          n__val_rml_33
                                                          start_sig__sig_34
                                                          end_sig__sig_35
                                                    ))
                                                Lco_ctrl_tree_record.rml_nothing)
                                               ::
                                               ((Lco_ctrl_tree_record.rml_await_immediate'
                                                  end_sig__sig_35)
                                                 ::
                                                 ((Lco_ctrl_tree_record.rml_compute
                                                    (function
                                                      | ()  ->
                                                          Lco_ctrl_tree_record.rml_expr_emit
                                                            start_sig__sig_34
                                                      ))
                                                   :: ([]))))
                                       )
                               )
                       )
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let rec repeat_propagation =
          (function
            | rounds__val_rml_37  ->
                (function
                  | n__val_rml_38  ->
                      ((function
                         | ()  ->
                             Lco_ctrl_tree_record.rml_if
                               (function
                                 | ()  -> Stdlib.(<=) rounds__val_rml_37 (0)
                                 )
                               (Lco_ctrl_tree_record.rml_compute
                                 (function | ()  -> () ))
                               (Lco_ctrl_tree_record.rml_seq
                                 (Lco_ctrl_tree_record.rml_run
                                   (function
                                     | ()  -> bench_propagation n__val_rml_38
                                     ))
                                 (Lco_ctrl_tree_record.rml_if
                                   (function
                                     | ()  ->
                                         Stdlib.(<=) rounds__val_rml_37 1
                                     )
                                   (Lco_ctrl_tree_record.rml_compute
                                     (function | ()  -> () ))
                                   (Lco_ctrl_tree_record.rml_seq
                                     Lco_ctrl_tree_record.rml_pause
                                     (Lco_ctrl_tree_record.rml_run
                                       (function
                                         | ()  ->
                                             repeat_propagation
                                               (Stdlib.(-)
                                                 rounds__val_rml_37 1)
                                               n__val_rml_38
                                         )))))
                         ):
                        (_) Lco_ctrl_tree_record.process)
                  )
            ) 
;;
let bench_propagation_multi =
      (function
        | n__val_rml_40  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_def
                     (function | ()  -> effective_multi_rounds n__val_rml_40
                       )
                     (function
                       | rounds__val_rml_41  ->
                           Lco_ctrl_tree_record.rml_run
                             (function
                               | ()  ->
                                   repeat_propagation
                                     rounds__val_rml_41 n__val_rml_40
                               )
                       )
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let observer =
      (function
        | trigger__val_rml_43  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_await_immediate'
                     trigger__val_rml_43
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let rec observers =
          (function
            | n__val_rml_45  ->
                (function
                  | trigger__val_rml_46  ->
                      ((function
                         | ()  ->
                             Lco_ctrl_tree_record.rml_if
                               (function
                                 | ()  -> Stdlib.(<=) n__val_rml_45 (0) )
                               (Lco_ctrl_tree_record.rml_compute
                                 (function | ()  -> () ))
                               (Lco_ctrl_tree_record.rml_par
                                 (Lco_ctrl_tree_record.rml_run
                                   (function
                                     | ()  -> observer trigger__val_rml_46 ))
                                 (Lco_ctrl_tree_record.rml_run
                                   (function
                                     | ()  ->
                                         observers
                                           (Stdlib.(-) n__val_rml_45 1)
                                           trigger__val_rml_46
                                     )))
                         ):
                        (_) Lco_ctrl_tree_record.process)
                  )
            ) 
;;
let bench_broadcast =
      (function
        | n__val_rml_48  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_def
                     (function | ()  -> effective_broadcast n__val_rml_48 )
                     (function
                       | n__val_rml_49  ->
                           Lco_ctrl_tree_record.rml_signal
                             (function
                               | trigger__sig_50  ->
                                   Lco_ctrl_tree_record.rml_par
                                     (Lco_ctrl_tree_record.rml_run
                                       (function
                                         | ()  ->
                                             observers
                                               n__val_rml_49 trigger__sig_50
                                         ))
                                     (Lco_ctrl_tree_record.rml_compute
                                       (function
                                         | ()  ->
                                             Lco_ctrl_tree_record.rml_expr_emit
                                               trigger__sig_50
                                         ))
                               )
                       )
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let rec fork_tree =
          (function
            | depth__val_rml_52  ->
                ((function
                   | ()  ->
                       Lco_ctrl_tree_record.rml_if
                         (function | ()  -> Stdlib.(<=) depth__val_rml_52 (0)
                           )
                         (Lco_ctrl_tree_record.rml_compute
                           (function | ()  -> () ))
                         (Lco_ctrl_tree_record.rml_seq
                           Lco_ctrl_tree_record.rml_pause
                           (Lco_ctrl_tree_record.rml_par
                             (Lco_ctrl_tree_record.rml_run
                               (function
                                 | ()  ->
                                     fork_tree
                                       (Stdlib.(-) depth__val_rml_52 1)
                                 ))
                             (Lco_ctrl_tree_record.rml_run
                               (function
                                 | ()  ->
                                     fork_tree
                                       (Stdlib.(-) depth__val_rml_52 1)
                                 ))))
                   ):
                  (_) Lco_ctrl_tree_record.process)
            ) 
;;
let bench_fork =
      (function
        | n__val_rml_54  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_run
                     (function
                       | ()  ->
                           fork_tree (effective_fork_depth n__val_rml_54)
                       )
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let rec guarded =
          (function
            | depth__val_rml_56  ->
                (function
                  | done_sig__val_rml_57  ->
                      ((function
                         | ()  ->
                             Lco_ctrl_tree_record.rml_if
                               (function
                                 | ()  -> Stdlib.(<=) depth__val_rml_56 (0) )
                               (Lco_ctrl_tree_record.rml_compute
                                 (function
                                   | ()  ->
                                       Lco_ctrl_tree_record.rml_expr_emit
                                         done_sig__val_rml_57
                                   ))
                               (Lco_ctrl_tree_record.rml_signal
                                 (function
                                   | guard__sig_58  ->
                                       Lco_ctrl_tree_record.rml_par
                                         (Lco_ctrl_tree_record.rml_when'
                                           guard__sig_58
                                           (Lco_ctrl_tree_record.rml_run
                                             (function
                                               | ()  ->
                                                   guarded
                                                     (Stdlib.(-)
                                                       depth__val_rml_56 1)
                                                     done_sig__val_rml_57
                                               )))
                                         (Lco_ctrl_tree_record.rml_compute
                                           (function
                                             | ()  ->
                                                 Lco_ctrl_tree_record.rml_expr_emit
                                                   guard__sig_58
                                             ))
                                   ))
                         ):
                        (_) Lco_ctrl_tree_record.process)
                  )
            ) 
;;
let bench_guarded =
      (function
        | n__val_rml_60  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_def
                     (function | ()  -> effective_guard_depth n__val_rml_60 )
                     (function
                       | depth__val_rml_61  ->
                           Lco_ctrl_tree_record.rml_signal
                             (function
                               | done_sig__sig_62  ->
                                   Lco_ctrl_tree_record.rml_par
                                     (Lco_ctrl_tree_record.rml_run
                                       (function
                                         | ()  ->
                                             guarded
                                               depth__val_rml_61
                                               done_sig__sig_62
                                         ))
                                     (Lco_ctrl_tree_record.rml_await_immediate'
                                       done_sig__sig_62)
                               )
                       )
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let rec repeat_guarded =
          (function
            | rounds__val_rml_64  ->
                (function
                  | n__val_rml_65  ->
                      ((function
                         | ()  ->
                             Lco_ctrl_tree_record.rml_if
                               (function
                                 | ()  -> Stdlib.(<=) rounds__val_rml_64 (0)
                                 )
                               (Lco_ctrl_tree_record.rml_compute
                                 (function | ()  -> () ))
                               (Lco_ctrl_tree_record.rml_seq
                                 (Lco_ctrl_tree_record.rml_run
                                   (function
                                     | ()  -> bench_guarded n__val_rml_65 ))
                                 (Lco_ctrl_tree_record.rml_if
                                   (function
                                     | ()  ->
                                         Stdlib.(<=) rounds__val_rml_64 1
                                     )
                                   (Lco_ctrl_tree_record.rml_compute
                                     (function | ()  -> () ))
                                   (Lco_ctrl_tree_record.rml_seq
                                     Lco_ctrl_tree_record.rml_pause
                                     (Lco_ctrl_tree_record.rml_run
                                       (function
                                         | ()  ->
                                             repeat_guarded
                                               (Stdlib.(-)
                                                 rounds__val_rml_64 1)
                                               n__val_rml_65
                                         )))))
                         ):
                        (_) Lco_ctrl_tree_record.process)
                  )
            ) 
;;
let bench_guarded_multi =
      (function
        | n__val_rml_67  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_def
                     (function | ()  -> effective_multi_rounds n__val_rml_67
                       )
                     (function
                       | rounds__val_rml_68  ->
                           Lco_ctrl_tree_record.rml_run
                             (function
                               | ()  ->
                                   repeat_guarded
                                     rounds__val_rml_68 n__val_rml_67
                               )
                       )
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let rec spin_forever =
          ((function
             | ()  ->
                 Lco_ctrl_tree_record.rml_seq
                   Lco_ctrl_tree_record.rml_pause
                   (Lco_ctrl_tree_record.rml_run
                     (function | ()  -> spin_forever ))
             ):
            (_) Lco_ctrl_tree_record.process) 
;;
let rec spinner =
          (function
            | steps__val_rml_71  ->
                ((function
                   | ()  ->
                       Lco_ctrl_tree_record.rml_if
                         (function | ()  -> Stdlib.(<=) steps__val_rml_71 (0)
                           )
                         (Lco_ctrl_tree_record.rml_compute
                           (function | ()  -> () ))
                         (Lco_ctrl_tree_record.rml_seq
                           Lco_ctrl_tree_record.rml_pause
                           (Lco_ctrl_tree_record.rml_run
                             (function
                               | ()  ->
                                   spinner (Stdlib.(-) steps__val_rml_71 1)
                               )))
                   ):
                  (_) Lco_ctrl_tree_record.process)
            ) 
;;
let rec nested_watch =
          (function
            | i__val_rml_73  ->
                (function
                  | depth__val_rml_74  ->
                      (function
                        | cancel_even__val_rml_75  ->
                            (function
                              | cancel_odd__val_rml_76  ->
                                  ((function
                                     | ()  ->
                                         Lco_ctrl_tree_record.rml_if
                                           (function
                                             | ()  ->
                                                 Stdlib.(>=)
                                                   i__val_rml_73
                                                   depth__val_rml_74
                                             )
                                           (Lco_ctrl_tree_record.rml_run
                                             (function
                                               | ()  ->
                                                   spinner
                                                     (Stdlib.(+)
                                                       depth__val_rml_74 3)
                                               ))
                                           (Lco_ctrl_tree_record.rml_def
                                             (function
                                               | ()  ->
                                                   if
                                                     Stdlib.(=)
                                                       (Stdlib.(mod)
                                                         i__val_rml_73 2)
                                                       (0)
                                                     then
                                                     cancel_even__val_rml_75
                                                     else
                                                     cancel_odd__val_rml_76
                                               )
                                             (function
                                               | cancel__val_rml_77  ->
                                                   Lco_ctrl_tree_record.rml_until'
                                                     cancel__val_rml_77
                                                     (Lco_ctrl_tree_record.rml_run
                                                       (function
                                                         | ()  ->
                                                             nested_watch
                                                               (Stdlib.(+)
                                                                 i__val_rml_73
                                                                 1)
                                                               depth__val_rml_74
                                                               cancel_even__val_rml_75
                                                               cancel_odd__val_rml_76
                                                         ))
                                               ))
                                     ):
                                    (_) Lco_ctrl_tree_record.process)
                              )
                        )
                  )
            ) 
;;
let killer =
      (function
        | cancel_even__val_rml_79  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_seq
                     Lco_ctrl_tree_record.rml_pause
                     (Lco_ctrl_tree_record.rml_emit' cancel_even__val_rml_79)
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let bench_preemption =
      (function
        | n__val_rml_81  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_def
                     (function
                       | ()  -> effective_preemption_depth n__val_rml_81 )
                     (function
                       | depth__val_rml_82  ->
                           Lco_ctrl_tree_record.rml_signal
                             (function
                               | cancel_even__sig_83  ->
                                   Lco_ctrl_tree_record.rml_signal
                                     (function
                                       | cancel_odd__sig_84  ->
                                           Lco_ctrl_tree_record.rml_par
                                             (Lco_ctrl_tree_record.rml_run
                                               (function
                                                 | ()  ->
                                                     nested_watch
                                                       (0)
                                                       depth__val_rml_82
                                                       cancel_even__sig_83
                                                       cancel_odd__sig_84
                                                 ))
                                             (Lco_ctrl_tree_record.rml_run
                                               (function
                                                 | ()  ->
                                                     killer
                                                       cancel_even__sig_83
                                                 ))
                                       )
                               )
                       )
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let selected =
      ((function
         | ()  ->
             Lco_ctrl_tree_record.rml_match
               (function | ()  -> Stdlib.(!) benchmark )
               (function
                 | "propagation_chains"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function | ()  -> bench_propagation (Stdlib.(!) size)
                         )
                 | "propagation_chains_multi"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function
                         | ()  -> bench_propagation_multi (Stdlib.(!) size) )
                 | "broadcast_expansion"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function | ()  -> bench_broadcast (Stdlib.(!) size) )
                 | "fork_explosion"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function | ()  -> bench_fork (Stdlib.(!) size) )
                 | "guarded_cascades"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function | ()  -> bench_guarded (Stdlib.(!) size) )
                 | "guarded_cascades_multi"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function
                         | ()  -> bench_guarded_multi (Stdlib.(!) size) )
                 | "nested_preemption"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function | ()  -> bench_preemption (Stdlib.(!) size)
                         )
                 | x__val_rml_86  ->
                     Lco_ctrl_tree_record.rml_compute
                       (function
                         | ()  ->
                             Stdlib.invalid_arg
                               (Stdlib.(^)
                                 "unknown benchmark: " x__val_rml_86)
                         )
                 )
         ):
        (_) Lco_ctrl_tree_record.process) 
;;
let peak_mb =
      (function
        | ()  ->
            (let st__val_rml_88 = Gc.stat ()  in
              Stdlib.(/.)
                (Stdlib.(/.)
                  (Stdlib.( *. )
                    (Stdlib.float_of_int (st__val_rml_88).Gc.heap_words)
                    (Stdlib.float_of_int Sys.word_size))
                  8.)
                (Stdlib.( *. ) 1024. 1024.))
        ) 
;;
let () =
      Rml_machine.rml_exec
        ([])
        ((function
           | ()  ->
               Lco_ctrl_tree_record.rml_seq
                 (Lco_ctrl_tree_record.rml_compute
                   (function | ()  -> parse_args (); Gc.full_major () ))
                 (Lco_ctrl_tree_record.rml_def
                   (function | ()  -> Unix.gettimeofday () )
                   (function
                     | t0__val_rml_89  ->
                         Lco_ctrl_tree_record.rml_seq
                           (Lco_ctrl_tree_record.rml_run
                             (function | ()  -> selected ))
                           (Lco_ctrl_tree_record.rml_compute
                             (function
                               | ()  ->
                                   (let t1__val_rml_90 = Unix.gettimeofday ()
                                      in
                                     Gc.full_major ();
                                       (let time_ms__val_rml_91 =
                                              Stdlib.( *. )
                                                (Stdlib.(-.)
                                                  t1__val_rml_90
                                                  t0__val_rml_89)
                                                1000.
                                          in
                                         let instants__val_rml_92 =
                                               instants_for
                                                 (Stdlib.(!) benchmark)
                                                 (Stdlib.(!) size)
                                            in
                                           let line__val_rml_93 =
                                                 Stdlib.(^)
                                                   "rml,"
                                                   (Stdlib.(^)
                                                     (Stdlib.(!) benchmark)
                                                     (Stdlib.(^)
                                                       ","
                                                       (Stdlib.(^)
                                                         (Stdlib.string_of_int
                                                           (Stdlib.(!) size))
                                                         (Stdlib.(^)
                                                           ","
                                                           (Stdlib.(^)
                                                             (Stdlib.string_of_int
                                                               (Stdlib.(!)
                                                                 run_id))
                                                             (Stdlib.(^)
                                                               ","
                                                               (Stdlib.(^)
                                                                 (Stdlib.string_of_float
                                                                   time_ms__val_rml_91)
                                                                 (Stdlib.(^)
                                                                   ","
                                                                   (Stdlib.(^)
                                                                    (Stdlib.string_of_int
                                                                    instants__val_rml_92)
                                                                    (Stdlib.(^)
                                                                    ","
                                                                    (Stdlib.string_of_float
                                                                    (peak_mb
                                                                    ()))))))))))))
                                              in
                                             Stdlib.print_endline
                                               line__val_rml_93;
                                               Stdlib.flush Stdlib.stdout))
                               ))
                     ))
           ):
          (_) Lco_ctrl_tree_record.process) 
;;
