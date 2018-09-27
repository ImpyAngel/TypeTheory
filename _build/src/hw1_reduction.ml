open Hw1
module StringSet = Set.Make(String);;
module StringMap = Map.Make(String);;

let lam = Abs("x", (App (Var "x", Var "y")))

let rec free_vars_set lam =
match lam with
| Var v -> StringSet.singleton v
| App (x, y) -> StringSet.union (free_vars_set x) (free_vars_set y)
| Abs (x, y) -> StringSet.remove x (free_vars_set y)


(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
let free_to_subst = failwith "Not implemented";;

(* Вернуть список имён свободных переменных *)
let free_vars x = StringSet.elements (free_vars_set x)


(* Проверить, находится ли лямбда-выражение в нормальной форме *)
let is_normal_form = failwith "Not implemented";;

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
let is_alpha_equivalent = failwith "Not implemented";;

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
let normal_beta_reduction = failwith "Not implemented";;

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать
   мемоизацию *)
let reduce_to_normal_form = failwith "Not implemented";;
