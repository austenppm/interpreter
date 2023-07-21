open Syntax
open Resultlib

(* 型環境 *)
type tyenv = ty Environment.t

(* 型代入 *)
type subst = (tyvar * ty) list

(* 制約条件 *)
type eqs = (ty * ty) list

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    v
  in
  body

let rec freevar_ty : ty -> tyvar MySet.t = function
  | TyInt | TyBool -> MySet.empty
  | TyVar tyvar -> MySet.singleton tyvar
  | TyFun (x, y) -> MySet.union (freevar_ty x) (freevar_ty y)
  | TyList t -> freevar_ty t

let subst_type subst ty : ty =
  let rec subst_single (tyvar_from, ty_to) ty_from : ty =
    match ty_from with
    | TyVar tyvar -> if tyvar == tyvar_from then ty_to else ty_from
    | TyFun (x, y) ->
        TyFun
          ( subst_single (tyvar_from, ty_to) x,
            subst_single (tyvar_from, ty_to) y )
    | TyList ty -> TyList (subst_single (tyvar_from, ty_to) ty)
    | _ -> ty_from
  in
  List.fold_left (fun acc_ty s -> subst_single s acc_ty) ty subst

let eqs_of_subst subst : eqs =
  List.map (fun (tyvar, ty) -> (TyVar tyvar, ty)) subst

let subst_eqs subst eqs : eqs =
  let single_subst_eqs (tyvar_from, ty_to) eqs =
    List.map
      (fun (ty1, ty2) ->
        ( subst_type [ (tyvar_from, ty_to) ] ty1,
          subst_type [ (tyvar_from, ty_to) ] ty2 ))
      eqs
  in
  List.fold_left (fun eqs s -> single_subst_eqs s eqs) eqs subst

(* 等式制約に対して単一化問題を解いて型代入を得る *)
let rec unify : eqs -> (subst, string) result = function
  | [] -> Ok []
  | eq :: eqs ->
      (match eq with
      | TyInt, TyInt | TyBool, TyBool -> unify eqs
      | TyFun (ty11, ty12), TyFun (ty21, ty22) ->
          unify ((ty11, ty21) :: (ty12, ty22) :: eqs)
      | TyVar alpha, ty | ty, TyVar alpha ->
          (* tyvar は ref 依存なので structural equality をチェックすべき *)
          if ty = TyVar alpha then unify eqs
          else if not (MySet.member alpha (freevar_ty ty)) then
            unify (subst_eqs [ (alpha, ty) ] eqs) >>= fun substs ->
            Ok ((alpha, ty) :: substs)
          else Error "Tyvar appears in FTV(ty)"
      | TyList ty1, TyList ty2 -> unify ((ty1, ty2) :: eqs)
      | _ -> Error "Types couldn't be matched")

let ty_prim op ty1 ty2 : eqs * ty =
  match op with
  | Plus | Mult -> ([ (ty1, TyInt); (ty2, TyInt) ], TyInt)
  | Lt -> ([ (ty1, TyInt); (ty2, TyInt) ], TyBool)
  | And | Or -> ([ (ty1, TyBool); (ty2, TyBool) ], TyBool)
  | Append ->
      (* ty2 がなんらかの list であるという制約条件を表現するために tyvar を新たに生成 *)
      let listty = TyVar (fresh_tyvar ()) in
      (* unity で ty2 が list であるという条件を先に見た方が効率が良い *)
      ([ (ty2, TyList listty); (ty1, listty) ], TyList listty)

let rec ty_exp' tyenv exp : (subst * ty, string) result =
  match exp with
  | Var x ->
      (match Environment.lookup x tyenv with
      | Some x -> Ok ([], x)
      | None -> Error ("Variable not bound: " ^ x))
  | ILit _ -> Ok ([], TyInt)
  | BLit _ -> Ok ([], TyBool)
  | Nil ->
      (* Nil の型は分からないので型変数を新しく生成 *)
      Ok ([], TyList (TyVar (fresh_tyvar ())))
  | BinOp (op, exp1, exp2) ->
      ty_exp' tyenv exp1 >>= fun (subst1, ty1) ->
      ty_exp' tyenv exp2 >>= fun (subst2, ty2) ->
      let eqs3, ty = ty_prim op ty1 ty2 in
      (* subst1 と subst2 を等式制約の集合に変換して、eqs3 と合わせる *)
      let eqs = eqs_of_subst subst1 @ eqs_of_subst subst2 @ eqs3 in
      (* 全体の制約をもう一度解く *)
      unify eqs >>= fun subst -> Ok (subst, subst_type subst ty)
  | IfExp (exp1, exp2, exp3) ->
      ty_exp' tyenv exp1 >>= fun (subst1, ty1) ->
      ty_exp' tyenv exp2 >>= fun (subst2, ty2) ->
      ty_exp' tyenv exp3 >>= fun (subst3, ty3) ->
      let eqs =
        eqs_of_subst subst1 @ eqs_of_subst subst2 @ eqs_of_subst subst3
        @ [
            (* 条件文の型は bool でなければならない *)
            (ty1, TyBool);
            (* then/else 文の型は一致しなければならない *)
            (ty2, ty3);
          ]
      in
      (* 全体の型は ty2 = ty3 になる *)
      unify eqs >>= fun subst -> Ok (subst, subst_type subst ty2)
  | LetExp (vars, exp2) ->
      (* tyenv の基で全ての宣言文を評価して tyenv に追加して返す *)
      let ty_decl_exp tyenv vars : (subst * tyenv, string) result =
        List.fold_left
          (fun r_acc (id, exp1) ->
            r_acc >>= fun (acc_subst, acc_tyenv) ->
            ty_exp' tyenv exp1 >>= fun (subst1, ty1) ->
            Ok (acc_subst @ subst1, Environment.extend id ty1 acc_tyenv))
          (Ok ([], tyenv))
          vars
      in
      ty_decl_exp tyenv vars >>= fun (subst1, newtyenv) ->
      (* exp1 の型を追加した型環境で exp2 の型を推論 *)
      ty_exp' newtyenv exp2 >>= fun (subst2, ty2) ->
      let eqs = eqs_of_subst subst1 @ eqs_of_subst subst2 in
      unify eqs >>= fun subst -> Ok (subst, subst_type subst ty2)
  | FunExp (id, exp) ->
      (* id の型を表す型変数を生成 *)
      let domty = TyVar (fresh_tyvar ()) in
      (* id : domty で tyenv を拡張し、その下で exp を型推論 *)
      ty_exp' (Environment.extend id domty tyenv) exp >>= fun (subst, ranty) ->
      Ok (subst, TyFun (subst_type subst domty, ranty))
  | LetRecExp (id, para, exp1, exp2) ->
      (* 関数の型を表す型変数を生成 *)
      let domty = TyVar (fresh_tyvar ()) in
      let ranty = TyVar (fresh_tyvar ()) in
      let exp1_env =
        Environment.extend id
          (TyFun (domty, ranty))
          (Environment.extend para domty tyenv)
      in
      ty_exp' exp1_env exp1 >>= fun (subst1, ty1) ->
      let exp2_env = Environment.extend id (TyFun (domty, ranty)) tyenv in
      ty_exp' exp2_env exp2 >>= fun (subst2, ty2) ->
      let eqs =
        eqs_of_subst subst1 @ eqs_of_subst subst2
        @ [
            (* exp1 の型は関数の値域の型に一致する *) (ty1, ranty);
          ]
      in
      unify eqs >>= fun subst -> Ok (subst, subst_type subst ty2)
  | AppExp (exp1, exp2) ->
      ty_exp' tyenv exp2 >>= fun (subst2, ty1) ->
      (* ty3 は ty1 -> ty2 なる関数の型 *)
      ty_exp' tyenv exp1 >>= fun (subst1, ty3) ->
      (* T-App に関する制約条件と、関数適用式全体の型 ty2 を求める *)
      let app_eqs, ty2 =
        match ty3 with
        (* exp1 が既に TyFun と判明している場合 *)
        | TyFun (ty1', ty2) -> ([ (ty1, ty1') ], ty2)
        (* そうでない場合、exp1 に TyFun 制約を新しくかけてあげる *)
        | _ ->
            (* exp1 : ty1' -> ty2 に表す *)
            let ty1' = TyVar (fresh_tyvar ()) in
            let ty2 = TyVar (fresh_tyvar ()) in
            ([ (ty3, TyFun (ty1', ty2)); (ty1, ty1') ], ty2)
      in
      let eqs = eqs_of_subst subst2 @ eqs_of_subst subst1 @ app_eqs in
      unify eqs >>= fun subst -> Ok (subst, subst_type subst ty2)
  | MatchExp (exp0, exp1, x_id, xs_id, exp2) ->
      (*
        match exp0 with [] -> exp1 | x::xs -> exp2

        exp0 : ty1 list
        x : ty1
        y : ty1 list
        exp1 : ty2
        exp2 : ty2
      *)
      ty_exp' tyenv exp0 >>= fun (subst0, ty0) ->
      ty_exp' tyenv exp1 >>= fun (subst1, ty2) ->
      (* まず exp0 の型を ty1 list に合わせるために ty1 を生成 *)
      let ty1 = TyVar (fresh_tyvar ()) in
      (* x と xs を入れた環境上で exp2 を評価 *)
      let exp2_tyenv =
        Environment.extend x_id ty1
          (Environment.extend xs_id (TyList ty1) tyenv)
      in
      ty_exp' exp2_tyenv exp2 >>= fun (subst2, ty2') ->
      let eqs =
        eqs_of_subst subst0 @ eqs_of_subst subst1 @ eqs_of_subst subst2
        @ [
            (* exp0 : ty1 list *)
            (ty0, TyList ty1);
            (* exp1 の型と exp2 の型が一致しないといけない *)
            (ty2, ty2');
          ]
      in
      unify eqs >>= fun subst -> Ok (subst, subst_type subst ty2)

(* `subst * ty` でなく `ty` だけを返してくれる ty_exp *)
let ty_exp tyenv exp : (ty, string) result = Result.map snd (ty_exp' tyenv exp)
