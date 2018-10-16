open Hw1
module StringSet = Set.Make(String);;
module StringMap = Map.Make(String);;

let lam = Abs("x", (App (Var "x", Var "y")))

let rec free_vars_set lam =
 match lam with
 | Var v -> StringSet.singleton v
 | App (x, y) -> StringSet.union (free_vars_set x) (free_vars_set y)
 | Abs (x, y) -> StringSet.remove x (free_vars_set y)


let expr_after_subst what source var =  
 match soure with 
 | Var v -> if (v = var) then Var v else what
 | App (x, y) -> App (expr_after_subst x, expr_after_subst y)
 | Abs (x, lambda) -> if (x = var) then Abs (x, lambda) else Abs (x, expr_after_subst y)


(** Returns `dest`, where all free `key`s replaced by `src`
    or throws an error, if `src` is not free for substitution *)
let substitute src dest key =
  let src_free_vars = free_var_set src in
  let no_key_to_replace x = not (has_free key x) in
  let not_bounding x = not (StringSet.mem x src_free_vars) in
  let str = string_of_lambda in
  let error() = "'" ^ (str src) ^ "' is not free for substitution in '" ^
                (str dest) ^ "' instead of '" ^ key ^ "'" in
  let rec substitute_rec dest =
    match dest with
    | Var v -> if (v = key) then src else dest
    | App (x, y) -> App(substitute_rec x, substitute_rec y);
    | Abs (x, y) when no_key_to_replace dest -> dest
    | Abs (x, y) when not_bounding x -> Abs (x, substitute_rec y)
    | _ -> failwith (error())
  in substitute_rec dest;;

(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
let free_to_subst src dest key = 
  try
    let _ = substitute src dest key in true
  with _ -> false;;
(* Вернуть список имён свободных переменных *)
let free_vars x = StringSet.elements (free_vars_set x)

(* Проверить, находится ли лямбда-выражение в нормальной форме *)
let rec is_normal_form lambda = match lambda with
    | App (Abs (tmp0, tmp1), tmp2) -> false
    | Var tmp -> true
    | App (tmp0, tmp1) -> (is_normal_form tmp0) && (is_normal_form tmp1)
    | Abs (tmp0, tmp1) -> is_normal_form tmp1;;

let rec all_vars_set lam =
 match lam with
 | Var v -> StringSet.singleton v
 | App (x, y) -> StringSet.union (all_vars_set x) (all_vars_set y)
 | Abs (x, y) -> StringSet.union (StringSet.singleton x) (all_vars_set y)

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
let is_alpha_equivalent first second = 
 let rec is_alpha_help first second vars = match (first, second) with
 | (Var v1, Var v2) -> (v1 = v2)
 | ((App x1, y1), (App x2, y2)) -> (is_alpha_help x1 x2 vars) && (is_alpha_help y1 y2 vars)
 | (Abs var1 lam1) (Abs var2 lam2) =
  let new_var = vars.max_elt ^ "#" in 
   is_alpha_help (substitute new_var lam1 x1) (substitute new_var lam2 x2) (vars @ new_var)
 in is_alpha_help first second (StringSet.union (all_vars_set first) (all_vars_set second))
 

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
let normal_beta_reduction lambda =
	if (is_normal_form lambda) then failwith "expression " ^ lambda ^ " in normal form yet" else 
	match lambda with
	 | Var v -> 
	 | 



(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать
   мемоизацию *)
let reduce_to_normal_form = failwith "Not implemented";;
