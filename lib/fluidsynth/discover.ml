module C = Configurator.V1

let () =
  C.main ~name:"tempo_fluidsynth" (fun c ->
      let default = { C.Pkg_config.libs = [ "-lfluidsynth" ]; cflags = [] } in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc -> (
            match C.Pkg_config.query pc ~package:"fluidsynth" with
            | None -> default
            | Some conf -> conf)
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
