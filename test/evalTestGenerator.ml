open Testutil
open Miniml
open Miniml.Eval
open Miniml.Syntax
open Miniml.Resultlib
open Miniml.Listlib

type evaluatedcase = {
  input : string;
  expected : exval;
}

type errorcase = { input : string }

type eval_to_raise_result =
  | Evaluated of exval
  | ErrorRaised

(*
  Miniml.Cui.eval_decl は tyenv も必要なので ty に関わるコードを抜いたバージョンを別にここにおく
*)
let eval_decl : env -> program -> ((id * exval) list * env, string) result =
 fun env -> function
  | Exp e -> eval_exp env e >>= fun v -> Ok ([ ("-", v) ], env)
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
            acc >>= fun (var_list, acc_env) ->
            (* 一つの Let 文の中の複数の代入文はお互い依存しないので常に最初の env を使う *)
            eval_exp env exp >>= fun v ->
            Ok ((id, v) :: var_list, Environment.extend id v acc_env))
          (Ok ([], env))
          ls
  | RecDecl (id, p, e) ->
      (* 型推論は未実装 *)
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV (p, e, dummyenv)) env in
      dummyenv := newenv;
      eval_exp newenv (FunExp (id, e)) >>= fun v -> Ok ([ (id, v) ], newenv)

let eval input =
  Exec.exec
    (fun env program ->
      let res = eval_decl env program in
      match res with
      | Ok (vars, newenv) -> (newenv, snd (List.hd (List.rev vars)))
      | Error e -> raise (Failure e))
    Miniml.Cui.initial_env input

let eval_to_raise src =
  try
    let value = eval src in
    Evaluated value
  with
  | Exec.Error msg -> raise (Exec.Error msg)
  | _ -> ErrorRaised

let eval_to_raise_result_printer = function
  | Evaluated value -> string_of_exval value
  | ErrorRaised -> "error"

let gen_eval_tests (dataset : evaluatedcase list) =
  gen_tests ~ishow:(fun x -> x) ~oshow:string_of_exval ~cmp:( = ) ~exec:eval
  @@ List.map
       (fun (testcase : evaluatedcase) : (string, exval) test ->
         { input = testcase.input; expected = testcase.expected })
       dataset

let gen_evalerror_tests (dataset : errorcase list) =
  gen_tests
    ~ishow:(fun x -> x)
    ~oshow:eval_to_raise_result_printer ~cmp:( = ) (* TODO *)
    ~exec:eval_to_raise
  @@ List.map
       (fun (testcase : errorcase) : (string, eval_to_raise_result) test ->
         { input = testcase.input; expected = ErrorRaised })
       dataset
