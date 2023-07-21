open Miniml

exception Error of string

let exec fn initial_env input =
  let lexbuf = LexingHelper.from_string_without_eof input in
  let env = ref initial_env in
  let result = ref None in
  (try
     while true do
       let programs = Parser.toplevel Lexer.main lexbuf in
       List.hd
         (List.map
            (fun program ->
              let new_env, value = fn !env program in
              result := Some value;
              env := new_env)
            programs)
     done
   with End_of_file -> ());
  match !result with
  | Some value -> value
  | None ->
      raise
        (Error (Printf.sprintf "lexer error: reached eof of input \"%s\"" input))
