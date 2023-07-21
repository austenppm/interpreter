(* ML interpreter / type reconstruction *)
<<<<<<< HEAD
exception Error of string

(* エラーを定義するための関数 *)
let err s = raise (Error s)

(* 変数・パラメータを表す型 *)
type id = string

(* 二項演算子を表す型 *)
type binOp = Plus | Mult | Lt | Andand | Barbar
=======
open MySet


exception Error of string
let err s = raise (Error s) 

type id = string

type binOp = Plus | Mult | Lt | And | Or
>>>>>>> MLinter

(* 式の抽象構文木を表す型 *)
type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
<<<<<<< HEAD
  | LetExp of id * exp * exp
  | FunExp of id * exp
  (* 動的束縛の構文を表す型 *)
  | DFunExp of id * exp
  | AppExp of exp * exp
  (* let rec式の構文です。 *)
  | LetRecExp of id * id * exp * exp
  | NilExp
  | ConsExp of exp * exp
  | MatchExp of exp * exp * id * id * exp

(* 宣言を含む抽象構文木を表す型 *)
type program =
  | Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp
  (* 構文解析の段階でエラーが出た場合の値です。 *)
  | Exception of id

(* 型変数を表す型。次々に新しく型変数を定義できるように int で管理する。 *)
type tyvar = int

(* 型を表す型 *)
type ty = TyInt | TyBool | TyVar of tyvar | TyFun of ty * ty | TyList of ty

(* 式中の型変数を全て求める関数。freevar って名前だし自由変数かも？ *)
let rec freevar_ty ty =
  match ty with
  (* TyIntとTyBoolは型変数ではないので空集合を返します。 *)
  | TyInt -> MySet.empty
  | TyBool -> MySet.empty
  (* TyVarは型変数なので、そのままそれを唯一の要素とする集合を返します。 *)
  | TyVar t -> MySet.singleton t
  (* TyFunの引数中に出現する全ての型変数の集合の和集合を返します。 *)
  | TyFun (l, r) -> MySet.union (freevar_ty l) (freevar_ty r)
  | TyList t -> freevar_ty t

(* 型を出力する際に使う関数 *)
let rec string_of_ty ty =
  match ty with
  (* TyIntとTyBoolはそのままstring型のint,boolにします。 *)
  | TyInt -> "int"
  | TyBool -> "bool"
  (* TyVarはint型であり、0と"'a"の対応から始まり、25と"'z"まで行くと、26からは"'a1"、"'b1"...となるから、TyVarの値を26で割り、余りに対応したアルファベットと
     商の値を連結したものを返します。 *)
  | TyVar t ->
      let rec string_of_free k =
        match k with
        | 0 -> "'a"
        | 1 -> "'b"
        | 2 -> "'c"
        | 3 -> "'d"
        | 4 -> "'e"
        | 5 -> "'f"
        | 6 -> "'g"
        | 7 -> "'h"
        | 8 -> "'i"
        | 9 -> "'j"
        | 10 -> "'k"
        | 11 -> "'l"
        | 12 -> "'m"
        | 13 -> "'n"
        | 14 -> "'o"
        | 15 -> "'p"
        | 16 -> "'q"
        | 17 -> "'r"
        | 18 -> "'s"
        | 19 -> "'t"
        | 20 -> "'u"
        | 21 -> "'v"
        | 22 -> "'w"
        | 23 -> "'x"
        | 24 -> "'y"
        | 25 -> "'z"
        | n -> string_of_free (n mod 26) ^ string_of_int (n / 26)
      in
      string_of_free t
  (* 右結合で"（第一引数の型） -> （第二引数の型）"を返します。 *)
  | TyFun (l, r) -> (
      match l with
      | TyFun (_, _) -> "(" ^ string_of_ty l ^ ")" ^ " -> " ^ string_of_ty r
      | _ -> string_of_ty l ^ " -> " ^ string_of_ty r)
  | TyList t -> string_of_ty t ^ " list"

(* 型を出力する関数 *)
let pp_ty ty =
  (* 引数をstring_of_tyに通してstring型にしてから出力します。 *)
  print_string (string_of_ty ty)

(* 新しい型変数を作る関数 *)
let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    v
  in
  body
=======
  | LetExp of (id * exp) list * exp 
  | FunExp of id * exp   
  | AppExp of exp * exp 
  (* | InfixExp of binOp  *)
  | DFunExp of id * exp 
  | LetRecExp of id * id * exp * exp 


type program =
    Exp of exp
  | Decl of (id * exp) list 
  | RecDecl of id * id * exp 
  | QuitDecl  



(* let f x1 x2 ...= expr => let f = fun x1 x2 ...-> expr => let f = fun x1 -> fun x2 ...-> expr *)
let rec argstoFun args e = match args with
   [] -> e
  | arg :: rest -> FunExp (arg, argstoFun rest e)

type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty

type tysc = TyScheme of tyvar list * ty 

let tysc_of_ty ty = TyScheme ([], ty) 
  
let rec freevar_ty = function   
    TyInt | TyBool -> MySet.empty
  | TyVar tv -> MySet.singleton tv
  | TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)
  (* | TyList _ -> (Todo) *)
  | _ -> err ("Not Implemented!")

let rec freevar_tysc = function 
  TyScheme (vars, ty) -> 
    let allvars = freevar_ty ty in
     MySet.diff allvars (MySet.from_list vars)

let rec string_of_ty  = function 
| TyInt -> "int"
| TyBool -> "bool"
| TyVar tv -> (* (0,1,2,...,25,26,...,51,...) => (a,b,c,...,z,a1,...,z1,...)*)
     let m = tv / 26 in
     let n = tv mod 26 in
     if m = 0 then  "'" ^ Char.escaped (char_of_int (n + 97))
     else "'" ^ Char.escaped (char_of_int (n+97)) ^ string_of_int m
| TyFun (ty1, ty2) ->
  (match ty1 with
     TyFun _ -> "(" ^ (string_of_ty ty1) ^ ") -> " ^ (string_of_ty ty2)
   | _ -> (string_of_ty ty1) ^ " -> " ^ (string_of_ty ty2))
| _ -> "Not Implemented!"


let rec pp_ty ty = print_string (string_of_ty ty) 



let var_of_binop  = function 
  Plus -> Var "+"
| Mult -> Var "*"
| Lt -> Var "<"
| And -> Var "&&"
| Or -> Var "||"

let id_of_binop = function 
  Var "+" -> "+"
| Var "*" -> "*"
| Var "<" -> "<"
| Var "&&" -> "&&"
| Var "||" -> "||"
| _ -> "Not Implemented!"

let fresh_tyvar = 
  let counter = ref 0 in (* 次に返すべき tyvar 型の値を参照で持ってく *)
  let body () =
    let v = !counter in
      counter := v + 1; v (* 呼び出されたら参照をインクリメントして，古い counter の参照先の値を返す *)
  in body
>>>>>>> MLinter
