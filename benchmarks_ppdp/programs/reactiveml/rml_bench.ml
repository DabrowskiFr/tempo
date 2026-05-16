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
let instants_for =
      (function
        | bench__val_rml_16  ->
            (function
              | n__val_rml_17  ->
                  (match bench__val_rml_16 with | "broadcast_expansion"  -> 2
                   | "fork_explosion"  -> effective_fork_depth n__val_rml_17
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
        | s_in__val_rml_21  ->
            (function
              | s_out__val_rml_22  ->
                  ((function
                     | ()  ->
                         Lco_ctrl_tree_record.rml_seq
                           (Lco_ctrl_tree_record.rml_await_immediate'
                             s_in__val_rml_21)
                           (Lco_ctrl_tree_record.rml_emit' s_out__val_rml_22)
                     ):
                    (_) Lco_ctrl_tree_record.process)
              )
        ) 
;;
let rec chain =
          (function
            | n__val_rml_24  ->
                (function
                  | s_in__val_rml_25  ->
                      (function
                        | s_out__val_rml_26  ->
                            ((function
                               | ()  ->
                                   Lco_ctrl_tree_record.rml_if
                                     (function
                                       | ()  -> Stdlib.(<=) n__val_rml_24 (0)
                                       )
                                     (Lco_ctrl_tree_record.rml_compute
                                       (function
                                         | ()  ->
                                             Lco_ctrl_tree_record.rml_expr_emit
                                               s_out__val_rml_26
                                         ))
                                     (Lco_ctrl_tree_record.rml_signal
                                       (function
                                         | mid__sig_27  ->
                                             Lco_ctrl_tree_record.rml_par
                                               (Lco_ctrl_tree_record.rml_run
                                                 (function
                                                   | ()  ->
                                                       link
                                                         s_in__val_rml_25
                                                         mid__sig_27
                                                   ))
                                               (Lco_ctrl_tree_record.rml_run
                                                 (function
                                                   | ()  ->
                                                       chain
                                                         (Stdlib.(-)
                                                           n__val_rml_24 1)
                                                         mid__sig_27
                                                         s_out__val_rml_26
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
        | n__val_rml_29  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_def
                     (function | ()  -> effective_propagation n__val_rml_29 )
                     (function
                       | n__val_rml_30  ->
                           Lco_ctrl_tree_record.rml_signal
                             (function
                               | start_sig__sig_31  ->
                                   Lco_ctrl_tree_record.rml_signal
                                     (function
                                       | end_sig__sig_32  ->
                                           Lco_ctrl_tree_record.rml_par_n
                                             ((Lco_ctrl_tree_record.rml_seq
                                                (Lco_ctrl_tree_record.rml_run
                                                  (function
                                                    | ()  ->
                                                        chain
                                                          n__val_rml_30
                                                          start_sig__sig_31
                                                          end_sig__sig_32
                                                    ))
                                                Lco_ctrl_tree_record.rml_nothing)
                                               ::
                                               ((Lco_ctrl_tree_record.rml_await_immediate'
                                                  end_sig__sig_32)
                                                 ::
                                                 ((Lco_ctrl_tree_record.rml_compute
                                                    (function
                                                      | ()  ->
                                                          Lco_ctrl_tree_record.rml_expr_emit
                                                            start_sig__sig_31
                                                      ))
                                                   :: ([]))))
                                       )
                               )
                       )
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let observer =
      (function
        | trigger__val_rml_34  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_await_immediate'
                     trigger__val_rml_34
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let rec observers =
          (function
            | n__val_rml_36  ->
                (function
                  | trigger__val_rml_37  ->
                      ((function
                         | ()  ->
                             Lco_ctrl_tree_record.rml_if
                               (function
                                 | ()  -> Stdlib.(<=) n__val_rml_36 (0) )
                               (Lco_ctrl_tree_record.rml_compute
                                 (function | ()  -> () ))
                               (Lco_ctrl_tree_record.rml_par
                                 (Lco_ctrl_tree_record.rml_run
                                   (function
                                     | ()  -> observer trigger__val_rml_37 ))
                                 (Lco_ctrl_tree_record.rml_run
                                   (function
                                     | ()  ->
                                         observers
                                           (Stdlib.(-) n__val_rml_36 1)
                                           trigger__val_rml_37
                                     )))
                         ):
                        (_) Lco_ctrl_tree_record.process)
                  )
            ) 
;;
let bench_broadcast =
      (function
        | n__val_rml_39  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_def
                     (function | ()  -> effective_broadcast n__val_rml_39 )
                     (function
                       | n__val_rml_40  ->
                           Lco_ctrl_tree_record.rml_signal
                             (function
                               | trigger__sig_41  ->
                                   Lco_ctrl_tree_record.rml_par
                                     (Lco_ctrl_tree_record.rml_run
                                       (function
                                         | ()  ->
                                             observers
                                               n__val_rml_40 trigger__sig_41
                                         ))
                                     (Lco_ctrl_tree_record.rml_compute
                                       (function
                                         | ()  ->
                                             Lco_ctrl_tree_record.rml_expr_emit
                                               trigger__sig_41
                                         ))
                               )
                       )
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let rec fork_tree =
          (function
            | depth__val_rml_43  ->
                ((function
                   | ()  ->
                       Lco_ctrl_tree_record.rml_if
                         (function | ()  -> Stdlib.(<=) depth__val_rml_43 (0)
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
                                       (Stdlib.(-) depth__val_rml_43 1)
                                 ))
                             (Lco_ctrl_tree_record.rml_run
                               (function
                                 | ()  ->
                                     fork_tree
                                       (Stdlib.(-) depth__val_rml_43 1)
                                 ))))
                   ):
                  (_) Lco_ctrl_tree_record.process)
            ) 
;;
let bench_fork =
      (function
        | n__val_rml_45  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_run
                     (function
                       | ()  ->
                           fork_tree (effective_fork_depth n__val_rml_45)
                       )
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let rec guarded =
          (function
            | depth__val_rml_47  ->
                (function
                  | done_sig__val_rml_48  ->
                      ((function
                         | ()  ->
                             Lco_ctrl_tree_record.rml_if
                               (function
                                 | ()  -> Stdlib.(<=) depth__val_rml_47 (0) )
                               (Lco_ctrl_tree_record.rml_compute
                                 (function
                                   | ()  ->
                                       Lco_ctrl_tree_record.rml_expr_emit
                                         done_sig__val_rml_48
                                   ))
                               (Lco_ctrl_tree_record.rml_signal
                                 (function
                                   | guard__sig_49  ->
                                       Lco_ctrl_tree_record.rml_par
                                         (Lco_ctrl_tree_record.rml_when'
                                           guard__sig_49
                                           (Lco_ctrl_tree_record.rml_run
                                             (function
                                               | ()  ->
                                                   guarded
                                                     (Stdlib.(-)
                                                       depth__val_rml_47 1)
                                                     done_sig__val_rml_48
                                               )))
                                         (Lco_ctrl_tree_record.rml_compute
                                           (function
                                             | ()  ->
                                                 Lco_ctrl_tree_record.rml_expr_emit
                                                   guard__sig_49
                                             ))
                                   ))
                         ):
                        (_) Lco_ctrl_tree_record.process)
                  )
            ) 
;;
let bench_guarded =
      (function
        | n__val_rml_51  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_def
                     (function | ()  -> effective_guard_depth n__val_rml_51 )
                     (function
                       | depth__val_rml_52  ->
                           Lco_ctrl_tree_record.rml_signal
                             (function
                               | done_sig__sig_53  ->
                                   Lco_ctrl_tree_record.rml_par
                                     (Lco_ctrl_tree_record.rml_run
                                       (function
                                         | ()  ->
                                             guarded
                                               depth__val_rml_52
                                               done_sig__sig_53
                                         ))
                                     (Lco_ctrl_tree_record.rml_await_immediate'
                                       done_sig__sig_53)
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
            | steps__val_rml_56  ->
                ((function
                   | ()  ->
                       Lco_ctrl_tree_record.rml_if
                         (function | ()  -> Stdlib.(<=) steps__val_rml_56 (0)
                           )
                         (Lco_ctrl_tree_record.rml_compute
                           (function | ()  -> () ))
                         (Lco_ctrl_tree_record.rml_seq
                           Lco_ctrl_tree_record.rml_pause
                           (Lco_ctrl_tree_record.rml_run
                             (function
                               | ()  ->
                                   spinner (Stdlib.(-) steps__val_rml_56 1)
                               )))
                   ):
                  (_) Lco_ctrl_tree_record.process)
            ) 
;;
let rec nested_watch =
          (function
            | i__val_rml_58  ->
                (function
                  | depth__val_rml_59  ->
                      (function
                        | cancel_even__val_rml_60  ->
                            (function
                              | cancel_odd__val_rml_61  ->
                                  ((function
                                     | ()  ->
                                         Lco_ctrl_tree_record.rml_if
                                           (function
                                             | ()  ->
                                                 Stdlib.(>=)
                                                   i__val_rml_58
                                                   depth__val_rml_59
                                             )
                                           (Lco_ctrl_tree_record.rml_run
                                             (function
                                               | ()  ->
                                                   spinner
                                                     (Stdlib.(+)
                                                       depth__val_rml_59 3)
                                               ))
                                           (Lco_ctrl_tree_record.rml_def
                                             (function
                                               | ()  ->
                                                   if
                                                     Stdlib.(=)
                                                       (Stdlib.(mod)
                                                         i__val_rml_58 2)
                                                       (0)
                                                     then
                                                     cancel_even__val_rml_60
                                                     else
                                                     cancel_odd__val_rml_61
                                               )
                                             (function
                                               | cancel__val_rml_62  ->
                                                   Lco_ctrl_tree_record.rml_until'
                                                     cancel__val_rml_62
                                                     (Lco_ctrl_tree_record.rml_run
                                                       (function
                                                         | ()  ->
                                                             nested_watch
                                                               (Stdlib.(+)
                                                                 i__val_rml_58
                                                                 1)
                                                               depth__val_rml_59
                                                               cancel_even__val_rml_60
                                                               cancel_odd__val_rml_61
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
        | cancel_even__val_rml_64  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_seq
                     Lco_ctrl_tree_record.rml_pause
                     (Lco_ctrl_tree_record.rml_emit' cancel_even__val_rml_64)
               ):
              (_) Lco_ctrl_tree_record.process)
        ) 
;;
let bench_preemption =
      (function
        | n__val_rml_66  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_def
                     (function
                       | ()  -> effective_preemption_depth n__val_rml_66 )
                     (function
                       | depth__val_rml_67  ->
                           Lco_ctrl_tree_record.rml_signal
                             (function
                               | cancel_even__sig_68  ->
                                   Lco_ctrl_tree_record.rml_signal
                                     (function
                                       | cancel_odd__sig_69  ->
                                           Lco_ctrl_tree_record.rml_par
                                             (Lco_ctrl_tree_record.rml_run
                                               (function
                                                 | ()  ->
                                                     nested_watch
                                                       (0)
                                                       depth__val_rml_67
                                                       cancel_even__sig_68
                                                       cancel_odd__sig_69
                                                 ))
                                             (Lco_ctrl_tree_record.rml_run
                                               (function
                                                 | ()  ->
                                                     killer
                                                       cancel_even__sig_68
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
                 | "broadcast_expansion"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function | ()  -> bench_broadcast (Stdlib.(!) size) )
                 | "fork_explosion"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function | ()  -> bench_fork (Stdlib.(!) size) )
                 | "guarded_cascades"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function | ()  -> bench_guarded (Stdlib.(!) size) )
                 | "nested_preemption"  ->
                     Lco_ctrl_tree_record.rml_run
                       (function | ()  -> bench_preemption (Stdlib.(!) size)
                         )
                 | x__val_rml_71  ->
                     Lco_ctrl_tree_record.rml_compute
                       (function
                         | ()  ->
                             Stdlib.invalid_arg
                               (Stdlib.(^)
                                 "unknown benchmark: " x__val_rml_71)
                         )
                 )
         ):
        (_) Lco_ctrl_tree_record.process) 
;;
let peak_mb =
      (function
        | ()  ->
            (let st__val_rml_73 = Gc.stat ()  in
              Stdlib.(/.)
                (Stdlib.(/.)
                  (Stdlib.( *. )
                    (Stdlib.float_of_int (st__val_rml_73).Gc.heap_words)
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
                     | t0__val_rml_74  ->
                         Lco_ctrl_tree_record.rml_seq
                           (Lco_ctrl_tree_record.rml_run
                             (function | ()  -> selected ))
                           (Lco_ctrl_tree_record.rml_compute
                             (function
                               | ()  ->
                                   (let t1__val_rml_75 = Unix.gettimeofday ()
                                      in
                                     Gc.full_major ();
                                       (let time_ms__val_rml_76 =
                                              Stdlib.( *. )
                                                (Stdlib.(-.)
                                                  t1__val_rml_75
                                                  t0__val_rml_74)
                                                1000.
                                          in
                                         let instants__val_rml_77 =
                                               instants_for
                                                 (Stdlib.(!) benchmark)
                                                 (Stdlib.(!) size)
                                            in
                                           let line__val_rml_78 =
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
                                                                   time_ms__val_rml_76)
                                                                 (Stdlib.(^)
                                                                   ","
                                                                   (Stdlib.(^)
                                                                    (Stdlib.string_of_int
                                                                    instants__val_rml_77)
                                                                    (Stdlib.(^)
                                                                    ","
                                                                    (Stdlib.string_of_float
                                                                    (peak_mb
                                                                    ()))))))))))))
                                              in
                                             Stdlib.print_endline
                                               line__val_rml_78;
                                               Stdlib.flush Stdlib.stdout))
                               ))
                     ))
           ):
          (_) Lco_ctrl_tree_record.process) 
;;
