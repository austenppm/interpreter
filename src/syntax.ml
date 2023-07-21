(* ML interpreter / type reconstruction *)
type id = string

type binOp =
  | Plus
  | Mult
  | Lt
  | And
  | Or
  | Append

let pp_binop = function
  | Plus -> "+"
  | Mult -> "*"
  | Lt -> "<"
  | And -> "&&"
  | Or -> "||"
  | Append -> "::"

type exp =
  | Var of id
  | ILit of int
  | BLit of bool
  | Nil
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of (id * exp) list * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp
  | MatchExp of exp * exp * id * id * exp

let rec pp_exp = function
  | Var id -> id
  | ILit i -> string_of_int i
  | BLit b -> string_of_bool b
  | Nil -> "[]"
  | BinOp (op, exp1, exp2) ->
      pp_exp exp1 ^ " " ^ pp_binop op ^ " " ^ pp_exp exp2
  | IfExp (exp1, exp2, exp3) ->
      "if " ^ pp_exp exp1 ^ " then " ^ pp_exp exp2 ^ " else " ^ pp_exp exp3
  | LetExp (vars, exp2) ->
      "let "
      ^ List.fold_left
          (fun acc (id, exp) ->
            acc
            ^ (if acc = "" then "" else "and")
            ^ " " ^ id ^ " = " ^ pp_exp exp)
          "" vars
      ^ " in " ^ pp_exp exp2
  | FunExp (id, exp) -> "fun " ^ id ^ " -> " ^ pp_exp exp
  | AppExp (exp1, exp2) -> "(" ^ pp_exp exp1 ^ " " ^ pp_exp exp2 ^ ")"
  | LetRecExp (id1, id2, exp1, exp2) ->
      "let rec " ^ id1 ^ " = fun " ^ id2 ^ " -> " ^ pp_exp exp1 ^ " in "
      ^ pp_exp exp2
  | MatchExp (exp, exp1, x, xs, exp2) ->
      "match " ^ pp_exp exp ^ " with [] -> " ^ pp_exp exp1 ^ " | " ^ x ^ "::"
      ^ xs ^ " -> " ^ pp_exp exp2

type program =
  | Exp of exp
  | Decl of (id * exp) list
  | RecDecl of id * id * exp

type tyvar = int

type ty =
  | TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty

let string_of_tyvar tyvar : string =
  "'"
  ^ String.make 1 (char_of_int (int_of_char 'a' + (tyvar mod 26)))
  ^ if tyvar < 26 then "" else string_of_int (tyvar / 26)

(*
  TyFun に関しては一番外側でのみ括弧がつかない。実質的な実装は string_of_ty' に置いて
  外側の string_of_ty には TyFun に関する（括弧がつかないような）処理のみを置いた。
*)
let string_of_ty ty : string =
  let rec string_of_ty' = function
    | TyInt -> "int"
    | TyBool -> "bool"
    | TyVar tyvar -> string_of_tyvar tyvar
    | TyFun (x, y) -> "(" ^ string_of_ty' x ^ " -> " ^ string_of_ty' y ^ ")"
    | TyList x -> string_of_ty' x ^ " list"
  in
  match ty with
  | TyFun (x, y) -> string_of_ty' x ^ " -> " ^ string_of_ty' y
  | _ -> string_of_ty' ty

let pp_ty = string_of_ty
