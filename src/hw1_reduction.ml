open Hw1;;

module StringSet = Set.Make(String);;
module StringMap = Map.Make(String);;

let lam = Abs("x", (App (Var "x", Var "y")))

let rec free_vars_set lam =
 match lam with
 | Var v -> StringSet.singleton v
 | App (x, y) -> StringSet.union (free_vars_set x) (free_vars_set y)
 | Abs (x, y) -> StringSet.remove x (free_vars_set y)


let rec expr_after_subst what source var =  
 match source with 
 | Var v -> if (v = var) then Var v else what
 | App (x, y) -> App (expr_after_subst what x var, expr_after_subst what y var)
 | Abs (x, lambda) -> if (x = var) then Abs (x, lambda) else Abs (x, expr_after_subst what lambda var)


let rec has_free key lambda =
  match lambda with
  | Var v -> v = key
  | App (x, y) -> (has_free key x) || (has_free key y)
  | Abs (x, y) when x = key -> false
  | Abs (x, y) -> has_free key y;;

(** Returns `dest`, where all free `key`s replaced by `src`
    or throws an error, if `src` is not free for substitution *)
let substitute src dest key =
  let rec substitute_rec dest =
    match dest with
| Var v -> if (v = key) then src else dest
    | App (x, y) -> App(substitute_rec x, substitute_rec y);
    | Abs (x, y) when not (has_free key dest) -> dest
    | Abs (x, y) when not (StringSet.mem x (free_vars_set src))  -> Abs (x, substitute_rec y)
    | _ -> failwith "not free for sub"
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
 | Abs (x, y) -> StringSet.union (StringSet.singleton x) (all_vars_set y);;

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
let is_alpha_equivalent first second = 
  let rec is_alpha_help first second vars = match (first, second) with
    | (Var v1, Var v2) -> (v1 = v2)
    | (App (x1, y1), App (x2, y2)) -> (is_alpha_help x1 x2 vars) && (is_alpha_help y1 y2 vars)
    | (Abs (var1, lam1), Abs (var2, lam2)) ->
      let new_var = (StringSet.max_elt vars) ^ "r" in 
        is_alpha_help (substitute (Var new_var) lam1 var1) (substitute (Var new_var) lam2 var2) (StringSet.add new_var vars)
    | _ -> false
  in is_alpha_help first second (StringSet.union (all_vars_set first) (all_vars_set second));;


(* для факторизации *)
let rec add_x lambda = match lambda with
 | Var v ->  Var ("x" ^ v) 
 | App (x, y) -> App (add_x x, add_x y)
 | Abs (x, lambda) -> Abs ("x" ^ x, add_x lambda);;

(* факторизация по эквивалентности *)
let rec alfa_equivalent_factorization lambda quntors = match lambda with
| Var v -> if (StringMap.mem v quntors) then Var (StringMap.find v quntors) else lambda
| App (first, second) -> App ((alfa_equivalent_factorization first quntors), (alfa_equivalent_factorization second quntors))
| Abs (v, lam) -> 
  let change_v = "y" ^ (string_of_int (StringMap.cardinal quntors)) in
  Abs (change_v, alfa_equivalent_factorization lam (StringMap.add v change_v quntors));;

let rec normal_beta_reduction_rec lambda = match lambda with
 | Var v -> None 
 | App (Abs (v, first), second) -> if (free_to_subst second first v) 
  then Some (substitute second first v)
  else (match normal_beta_reduction_rec first with
   | Some x -> Some (App (x, second))
   | _ -> match normal_beta_reduction_rec second with
    | Some y -> Some (App (first, y))
    | _ -> None)
 | App (first, second) ->
  (match normal_beta_reduction_rec first with
   | Some x -> Some (App (x, second))
   | _ -> match normal_beta_reduction_rec second with
    | Some y -> Some (App (first, y))
    | _ -> None)
 | Abs (v, lam) ->  
  match normal_beta_reduction_rec lam with
  | Some x -> Some (Abs (v, x))  
  | _ -> None;;

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
let normal_beta_reduction lambda = match normal_beta_reduction_rec lambda with
 | Some x -> x
 | _ -> lambda;;

let rec reduce_with_mem lambda preproc =
	let reduce_app first second = (let (norm_first, p) = reduce_with_mem first preproc in
  	 match norm_first with
  	 | Abs (v1, l1) -> reduce_with_mem (App (norm_first, second)) p
  	 | _ -> let (norm_second, p2) = reduce_with_mem second p in (App (norm_first, norm_second), p2)) in
	let to_unary k = string_of_lambda (alfa_equivalent_factorization k StringMap.empty) in
	let key = to_unary lambda in
	if (StringMap.mem key preproc) 
	 then (lambda_of_string (StringMap.find key preproc), preproc)
	 else let (value, new_preproc) = match lambda with
			 | Var v -> (lambda, preproc)
			 | App (Abs (v, first), second) -> if (free_to_subst second first v) 
			  then reduce_with_mem (substitute second first v) preproc
			  else reduce_app (Abs (v, first)) second
			 | App (first, second) -> reduce_app first second
			 | Abs (v, lam) -> let (norm_lam, p) = reduce_with_mem lam preproc in (Abs (v, norm_lam), p) in
		   (value, StringMap.add key (to_unary value) new_preproc)

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать
   меморизацию *)
let reduce_to_normal_form lambda = fst (reduce_with_mem (add_x lambda) StringMap.empty);;
	