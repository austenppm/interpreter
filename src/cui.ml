open Eval
open Typing
open Syntax
open Resultlib
open Listlib

(*
  与えられた env と program に対して、
  - 宣言文が正しい場合、新しく宣言された変数のリストとそれらが追加されて更新された newenv
  - 宣言文が正しくない場合、エラーメッセージ Error string
  を返す。

  新しく宣言された変数のリストを返すのは、REPLに出力するためだけである。
*)
let eval_decl :
    tyenv ->
    env ->
    program ->
    ((id * ty * exval) list * tyenv * env, string) result =
 fun tyenv env -> function
  | Exp e ->
      ty_exp tyenv e >>= fun ty ->
      eval_exp env e >>= fun v -> Ok ([ ("-", ty, v) ], tyenv, env)
  | Decl ls ->
      (* 一つの Let 文の中に同一 id に対する代入文が複数入っていたら Error を投げる *)
      let ids = List.map fst ls in
      if check_duplicates ids then
        Error "Duplicate ids in a single let statement"
      else
        (*
          複数の代入文を左から評価して集める。
          一つでも正しくない宣言文があった場合には全ての結果が Error になる
        *)
        List.fold_left
          (fun acc (id, exp) ->
            acc >>= fun (var_list, acc_tyenv, acc_env) ->
            (* 一つの Let 文の中の複数の代入文はお互い依存しないので常に最初の tyenv, env を使う *)
            ty_exp tyenv exp >>= fun ty ->
            eval_exp env exp >>= fun v ->
            Ok
              ( (id, ty, v) :: var_list,
                Environment.extend id ty acc_tyenv,
                Environment.extend id v acc_env ))
          (Ok ([], tyenv, env))
          ls
  | RecDecl (id, p, e) ->
      (* 型推論は未実装 *)
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV (p, e, dummyenv)) env in
      dummyenv := newenv;
      eval_exp newenv (FunExp (id, e)) >>= fun v ->
      Ok ([ (id, TyInt, v) ], tyenv, newenv)

let rec read_eval_print env tyenv =
  print_string "> ";
  flush stdout;
  match
    let r_decls =
      try Ok (Parser.toplevel Lexer.main (Lexing.from_channel stdin)) with
      | Failure e -> Error e
      | _ -> Error "Parse Error"
    in
    r_decls >>= fun decls ->
    (* fold_left で複数の program を評価しながら tyenv, env を更新していく *)
    List.fold_left
      (fun acc decl ->
        acc >>= fun (acc_tyenv, acc_env) ->
        eval_decl acc_tyenv acc_env decl >>= fun (var_list, newtyenv, newenv) ->
        let result_string =
          List.fold_left
            (fun acc (id, t, v) ->
              acc ^ "val " ^ id ^ " : " ^ pp_ty t ^ " = " ^ pp_exval v ^ "\n")
            "" var_list
        in
        print_string result_string;
        Ok (newtyenv, newenv))
      (Ok (tyenv, env))
      decls
  with
  | Ok (newtyenv, newenv) -> read_eval_print newenv newtyenv
  | Error e ->
      print_string e;
      print_newline ();
      read_eval_print env tyenv

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
       (Environment.extend "x" (IntV 10)
          (Environment.extend "ii" (IntV 2)
             (Environment.extend "iii" (IntV 3)
                (Environment.extend "iv" (IntV 4) Environment.empty)))))

let initial_tyenv =
  Environment.extend "i" TyInt
    (Environment.extend "v" TyInt
       (Environment.extend "x" TyInt Environment.empty))
