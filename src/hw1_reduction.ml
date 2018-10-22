open Hw1;;

module SS = Set.Make(String);;
module SM = Map.Make(String);;

let lam = Abs("x", (App (Var "x", Var "y")))

let rec free_vars_set lam =
 match lam with
 | Var v -> SS.singleton v
 | App (x, y) -> SS.union (free_vars_set x) (free_vars_set y)
 | Abs (x, y) -> SS.remove x (free_vars_set y)


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

let substitute src dest key =
  let rec substitute_rec dest =
    match dest with
| Var v -> if (v = key) then src else dest
    | App (x, y) -> App(substitute_rec x, substitute_rec y);
    | Abs (x, y) when not (has_free key dest) -> dest
    | Abs (x, y) when not (SS.mem x (free_vars_set src))  -> Abs (x, substitute_rec y)
    | _ -> failwith "not free for sub"
  in substitute_rec dest;;

(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
let free_to_subst src dest key = 
  try
    let _ = substitute src dest key in true
  with _ -> false;;

(* Вернуть список имён свободных переменных *)
let free_vars x = SS.elements (free_vars_set x)

(* Проверить, находится ли лямбда-выражение в нормальной форме *)
let rec is_normal_form lambda = match lambda with
    | App (Abs (tmp0, tmp1), tmp2) -> not (free_to_subst tmp2 tmp1 tmp0)
    | Var tmp -> true
    | App (tmp0, tmp1) -> (is_normal_form tmp0) && (is_normal_form tmp1)
    | Abs (tmp0, tmp1) -> is_normal_form tmp1;;

let rec all_vars_set lam =
 match lam with
 | Var v -> SS.singleton v
 | App (x, y) -> SS.union (all_vars_set x) (all_vars_set y)
 | Abs (x, y) -> SS.union (SS.singleton x) (all_vars_set y);;

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
let is_alpha_equivalent first second = 
  let rec is_alpha_help first second vars = match (first, second) with
    | (Var v1, Var v2) -> (v1 = v2)
    | (App (x1, y1), App (x2, y2)) -> (is_alpha_help x1 x2 vars) && (is_alpha_help y1 y2 vars)
    | (Abs (var1, lam1), Abs (var2, lam2)) ->
      let new_var = (SS.max_elt vars) ^ "r" in 
        is_alpha_help (substitute (Var new_var) lam1 var1) (substitute (Var new_var) lam2 var2) (SS.add new_var vars)
    | _ -> false
  in is_alpha_help first second (SS.union (all_vars_set first) (all_vars_set second));;


(* для факторизации *)
let rec add_x lambda = match lambda with
 | Var v ->  Var ("x" ^ v) 
 | App (x, y) -> App (add_x x, add_x y)
 | Abs (x, lambda) -> Abs ("x" ^ x, add_x lambda);;

(* факторизация по эквивалентности *)
let rec alfa_equivalent_factorization lambda quntors = match lambda with
| Var v -> if (SM.mem v quntors) then Var (SM.find v quntors) else lambda
| App (first, second) -> App ((alfa_equivalent_factorization first quntors), (alfa_equivalent_factorization second quntors))
| Abs (v, lam) -> 
  let change_v = "y" ^ (string_of_int (SM.cardinal quntors)) in
  Abs (change_v, alfa_equivalent_factorization lam (SM.add v change_v quntors));;

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
type arena_lambda = Var_arena of string
                | Abs_arena of (string * arena_lambda ref)
                | App_arena of (arena_lambda ref * arena_lambda ref);;

let rec ref_of_lambda lambda =
  match lambda with
  | Var v -> ref (Var_arena v)
  | App (x, y) -> ref (App_arena (ref_of_lambda x, ref_of_lambda y))
  | Abs (x, y) -> ref (Abs_arena (x, ref_of_lambda y));;

let rec lambda_of_arena arena_lambda =
  match !arena_lambda with
  | Var_arena v -> Var v
  | App_arena (x, y) -> App (lambda_of_arena x, lambda_of_arena y)
  | Abs_arena (x, y) -> Abs (x, lambda_of_arena y);;

let unique_var = Stream.from (fun i -> Some ("var" ^ string_of_int i));;

let rec reduction_step arena_lambda =
  let rec to_alpha_eq arena_lambda map =
    match !arena_lambda with
    | Var_arena v -> if SM.mem v map then ref (Var_arena (SM.find v map)) else arena_lambda
    | App_arena (x, y) -> ref (App_arena (to_alpha_eq x map, to_alpha_eq y map))
    | Abs_arena (x, y) ->
      let temp = Stream.next unique_var in
      ref (Abs_arena (temp, to_alpha_eq y (SM.add x temp map)))
  in
  let rec try_to_subst src dest key =
    match !dest with
    | Var_arena a -> if a = key then dest := !src
    | Abs_arena (a, b) -> if a <> key then try_to_subst src b key
    | App_arena (a, b) ->
      try_to_subst src a key;
      try_to_subst src b key
  in
  let reduction_app a b =
    match !a with
    | Abs_arena (x, y) ->
      let temp = Stream.next unique_var in
      arena_lambda := !(to_alpha_eq y (SM.singleton x temp));
      try_to_subst b arena_lambda temp;
      (arena_lambda, true)
    | _ ->
      match reduction_step a with
      | (_, true) -> (arena_lambda, true)
      | _ ->
        match reduction_step b with
        | (_, true) -> (arena_lambda, true)
        | _ -> (arena_lambda, false)
  in
  match !arena_lambda with
  | Var_arena a -> (ref (Var_arena a), false)
  | App_arena (a, b) -> reduction_app a b
  | Abs_arena (a, b) ->
    match reduction_step b with
    | (_, true) -> (arena_lambda, true)
    | _ -> (arena_lambda, false)

let normal_beta_reduction lambda =
  match reduction_step (ref_of_lambda lambda) with
  | (x, true) -> lambda_of_arena x
  | _ -> lambda;;

let reduce_to_normal_form lambda =
  let rec reduction arena_lambda =
    match reduction_step arena_lambda with
    | (x, true) -> reduction x
    | _ -> arena_lambda
  in
  let result = reduction (ref_of_lambda lambda) in
  lambda_of_arena result;;
	