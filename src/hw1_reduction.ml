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
| Var v -> if (v == var) then Var v else what
| App (x, y) -> App (expr_after_subst x, expr_after_subst y)
| Abs (x, y) -> Abs (x, expr_after_subst y)

(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
let free_to_subst what source var =
if (free_vars_set source == free_vars_set (expr_after_subst what source var)

(* Вернуть список имён свободных переменных *)
let free_vars x = StringSet.elements (free_vars_set x)

(* Проверить, находится ли лямбда-выражение в нормальной форме *)
let is_normal_form lam =
match lam with
| App (x, y) -> 
	mathch x with
	|  Abs (var, source) ->

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
let is_alpha_equivalent = failwith "Not implemented";;

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
let normal_beta_reduction = failwith "Not implemented";;

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать
   мемоизацию *)
let reduce_to_normal_form = failwith "Not implemented";;
