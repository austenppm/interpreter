open Syntax
open Resultlib
open Listlib

type exval =
  | IntV of int
  | BoolV of bool
  | ListV of exval list
  | ProcV of id * exp * dnval Environment.t ref

and dnval = exval

type env = exval Environment.t

let rec string_of_exval = function
  | IntV i -> "IntV " ^ string_of_int i
  | BoolV b -> "BoolV " ^ string_of_bool b
  | ListV l ->
      "ListV ("
      ^ List.fold_left
          (fun acc x ->
            acc ^ (if acc = "" then "" else ", ") ^ string_of_exval x)
          "" l
      ^ ")"
  | ProcV _ -> "ProcV"

let rec pp_exval = function
  | IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ListV l ->
      "["
      ^ List.fold_left
          (fun acc x -> acc ^ (if acc = "" then "" else "; ") ^ pp_exval x)
          "" l
      ^ "]"
  | ProcV _ -> "function"

let rec apply_prim op arg1 arg2 : (exval, string) result =
  match (op, arg1, arg2) with
  | Plus, IntV i1, IntV i2 -> Ok (IntV (i1 + i2))
  | Plus, _, _ -> Error "Both arguments must be integer: +"
  | Mult, IntV i1, IntV i2 -> Ok (IntV (i1 * i2))
  | Mult, _, _ -> Error "Both arguments must be integer: *"
  | Lt, IntV i1, IntV i2 -> Ok (BoolV (i1 < i2))
  | Lt, _, _ -> Error "Both arguments must be integer: <"
  | And, BoolV v1, BoolV v2 -> Ok (BoolV (v1 && v2))
  | And, _, _ -> Error "Both arguments must be boolean: &&"
  | Or, BoolV v1, BoolV v2 -> Ok (BoolV (v1 || v2))
  | Or, _, _ -> Error "Both arguments must be boolean: ||"
  | Append, x, ListV xs -> Ok (ListV (x :: xs))
  | Append, _, _ -> Error "Second argument must be a list: ::"

(* エラーが発生したときには Error string を返す *)
let rec eval_exp env exp : (exval, string) result =
  match exp with
  | Var x ->
      (match Environment.lookup x env with
      | Some x -> Ok x
      | None -> Error ("Variable not bound: " ^ x))
  | ILit i -> Ok (IntV i)
  | BLit b -> Ok (BoolV b)
  | Nil -> Ok (ListV [])
  | BinOp (op, exp1, exp2) ->
      eval_exp env exp1 >>= fun arg1 ->
      (* 2つ目の被演算子を評価する必要がない場合に対しての処理を先に行う *)
      (match (op, arg1) with
      | And, BoolV false -> Ok (BoolV false)
      | Or, BoolV true -> Ok (BoolV true)
      | _, _ -> eval_exp env exp2 >>= fun arg2 -> apply_prim op arg1 arg2)
  | IfExp (exp1, exp2, exp3) ->
      (* if の条件文 exp1 が正しい表現である場合にのみ exp2 か exp3 を評価する *)
      eval_exp env exp1 >>= fun test ->
      (match test with
      | BoolV true -> eval_exp env exp2
      | BoolV false -> eval_exp env exp3
      | _ -> Error "Test expression must be boolean: if")
  | LetExp (vars, exp2) ->
      (* 一つの Let 文の中に同一 id に対する代入文が複数入っていたら Error を投げる *)
      let ids = List.map fst vars in
      if check_duplicates ids then
        Error "Duplicate ids in a single let statement"
      else
        (* 複数の宣言文を評価し exp2 を評価するための newenv を構築する *)
        List.fold_left
          (fun r_acc (id, exp1) ->
            r_acc >>= fun acc_env ->
            (* ここの宣言文は全てお互い独立に処理されるので exp1 の評価には常に元々の env が使われる *)
            eval_exp env exp1 >>= fun value ->
            Ok (Environment.extend id value acc_env))
          (Ok env) vars
        >>= fun newenv -> eval_exp newenv exp2
  | FunExp (id, exp) -> Ok (ProcV (id, exp, ref env))
  | AppExp (exp1, exp2) ->
      (* 被演算子 exp1 は ProcV である場合にのみ exp2 を評価する *)
      eval_exp env exp1 >>= fun funval ->
      (match funval with
      | ProcV (id, body, env') ->
          eval_exp env exp2 >>= fun arg ->
          let newenv = Environment.extend id arg !env' in
          eval_exp newenv body
      | _ -> Error "Non-function value is applied")
  | LetRecExp (id, para, exp1, exp2) ->
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
      dummyenv := newenv;
      eval_exp newenv exp2
  | MatchExp (exp, exp1, x_id, xs_id, exp2) ->
      eval_exp env exp >>= fun exval ->
      (match exval with
      | ListV [] -> eval_exp env exp1
      | ListV (x :: xs) ->
          let newenv =
            Environment.extend x_id x (Environment.extend xs_id (ListV xs) env)
          in
          eval_exp newenv exp2
      | _ -> Error "Match statement only works with lists for now")
