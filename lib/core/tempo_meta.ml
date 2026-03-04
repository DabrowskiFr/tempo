let version_string = "0.1.0"

let api_level = 2

let require_api_level expected =
  if api_level < expected then
    invalid_arg
      (Printf.sprintf "Tempo API level %d required, found %d" expected api_level)
