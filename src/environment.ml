type 'a t = (Syntax.id * 'a) list

let to_string (f : 'a -> string) (env : 'a t) =
  let str =
    List.fold_left
      (fun str (id, a) ->
        str ^ (if str == "" then " " else "\n  ") ^ id ^ ": " ^ f a ^ ",")
      "" env
  in
  "[" ^ str ^ " ]\n"

let empty = []

let extend x v env = (x, v) :: env

let rec lookup x env = List.assoc_opt x env

let rec map f = function [] -> [] | (id, v) :: rest -> (id, f v) :: map f rest

let rec fold_right f env a =
  match env with [] -> a | (_, v) :: rest -> f v (fold_right f rest a)
