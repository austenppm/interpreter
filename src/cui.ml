open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
 try  
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (id, newenv, v) = eval_decl env decl in
  Printf.printf "val %s = " id;
  pp_val v;
  print_newline();
  read_eval_print newenv
with
  err ->
    let e = Printexc.to_string err in (*get error message*)
    print_string ("Error: " ^ e ^ "\n");
    read_eval_print env

let read_eval_print_from_file filename env =
  let in_channel = open_in filename in
  try
    let program = really_input_string in_channel (in_channel_length in_channel) in
    let decl = Parser.toplevel Lexer.main (Lexing.from_string program) in
    let (id, newenv, v) = Eval.eval_decl env decl in
    Printf.printf "val %s = " id;
    pp_val v;
    print_newline();
    newenv
  with e ->
    close_in in_channel;
    raise e

    
let initial_env =
  Environment.extend "iv" (IntV 4)
    (Environment.extend "iii" (IntV 3)
      (Environment.extend "ii" (IntV 2) Environment.empty))