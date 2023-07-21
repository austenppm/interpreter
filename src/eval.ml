open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ListV of exval list
  | ProcV of id * exp * dnval Environment.t ref 
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ListV l ->
    "ListV ("
    ^ List.fold_left
        (fun acc x ->
          acc ^ (if acc = "" then "" else ", ") ^ string_of_exval x)
        "" l
    ^ ")"
  | ProcV _ -> "function"
  
let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | Gt, IntV i1, IntV i2 -> BoolV (i1 > i2)
  | Gt, _, _ -> err ("Both arguments must be integer: >")


let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LogicOp (op, exp1, exp2) ->
        (match op with
        And ->
        let arg1 = eval_exp env exp1 in
        (match arg1 with
        BoolV (false) -> BoolV (false)
        | BoolV (true) -> let arg2 = eval_exp env exp2 in
        (match arg2 with
        BoolV (true) -> BoolV (true)
        | BoolV (false) -> BoolV (false)
        | _ -> err ("Both arguments must be boolean: &&"))
        | _ -> err ("Both arguments must be boolean: &&"))
        | Or ->
        let arg1 = eval_exp env exp1 in
        (match arg1 with
        BoolV (true) -> BoolV (true)
        | BoolV (false) -> let arg2 = eval_exp env exp2 in
        (match arg2 with
        BoolV (true) -> BoolV (true)
        | BoolV (false) -> BoolV (false)
        | _ -> err ("Both arguments must be boolean: ||"))
        | _ -> err ("Both arguments must be boolean: ||")))
   | LetExp (id, exp1, exp2) ->
     let value = eval_exp env exp1 in
     eval_exp (Environment.extend id value env) exp2
   | LetAndExp (decls, body) ->
  let ids = List.map fst decls in
  let has_duplicates =
    List.length ids <> List.length (List.sort_uniq compare ids)
  in
  if has_duplicates then
    err "Duplicate variable declaration in let ... and ..."
  else
    let values = List.map (fun (id, exp) -> (id, eval_exp env exp)) decls in
    let env' = List.fold_left (fun env (id, value) -> Environment.extend id value env) env values in
    eval_exp env' body
| FunExp (id, exp) -> let r_env = ref env in ProcV (id, exp, r_env)
| AppExp (exp1, exp2) ->
  let funval = eval_exp env exp1 in
  let arg = eval_exp env exp2 in
  (match funval with
  ProcV (id, body, r_env) -> 
  let newenv = Environment.extend id arg !r_env in
    eval_exp newenv body
  | _ ->
    err ("Non-function value is applied"))
  | LetRecExp (id, para, exp1, exp2) ->
    let dummyenv = ref Environment.empty in
    let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
        dummyenv := newenv;
        eval_exp newenv exp2
| LetRecExp (id, para, exp1, exp2) ->
    (* Create a reference to a dummy environment *)
    let dummyenv = ref Environment.empty in
    (* Create a function closure and extend the current environment env to map id to this function closure *)
    let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
    (* Update the dummy environment with the extended environment *)
    dummyenv := newenv;
    eval_exp newenv exp2

let eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | Decl (id, e) ->
      let v = eval_exp env e in (id, Environment.extend id v env, v)
| LetDecls decls ->
  let ids = List.map fst decls in
  let has_duplicates =
    List.length ids <> List.length (List.sort_uniq compare ids)
  in
  if has_duplicates then
    err "Duplicate variable declaration in let ... and ..."
  else
    let values = List.map (fun (id, exp) -> (id, eval_exp env exp)) decls in
    let env' = List.fold_left (fun env (id, value) -> Environment.extend id value env) env values in
    ("-", env', IntV 0)
| RecDecl (id_f, id_x, e) ->
let dummyenv = ref Environment.empty in
let newenv = Environment.extend id_f (ProcV (id_x, e, dummyenv)) env in
dummyenv := newenv;
(id_f, newenv, (ProcV (id_x, e, dummyenv)))
