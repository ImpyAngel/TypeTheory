open Hw1;;
open Hw1_reduction;;
open Hw2_unify;;

module SM = Map.Make(String);;
module SS = Set.Make(String);;

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type

type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type

let rec term_of_simp_type t =
  let to_term = term_of_simp_type in
  match t with
  | S_Elem v -> Var v
  | S_Arrow (a, b) -> Fun("->", [ (to_term a); (to_term b) ]);;

let rec string_of_s_type s_t = match s_t with
    | S_Elem name -> name
    | S_Arrow (s_t1, s_t2) -> "(" ^ (string_of_s_type s_t1) ^ " -> " ^ (string_of_s_type s_t2) ^ ")";;

let rec simp_type_of_term t =
  let to_type = simp_type_of_term in
  match t with
  | Var v -> S_Elem v
  | Fun(f, [ l; r ]) when f = "->" -> S_Arrow (to_type l, to_type r)
  | _ -> failwith "Fun is not arrow";;

let equation_of_types (l, r) = (term_of_simp_type l, term_of_simp_type r);;

let rec add_x lambda = match lambda with
 | App (x, y) -> App (add_x x, add_x y)
 | Var v ->  Var ("x" ^ v) 
 | Abs (x, lambda) -> Abs ("x" ^ x, add_x lambda);;

let new_types_stream = Stream.from (fun i -> Some ("type" ^ string_of_int i));;
let infer_simp_type lambda =
  let lambda = add_x lambda in
  let add_type_to_map map v = SM.add v (Stream.next new_types_stream) map in
  let rec to_system lambda types =
    match lambda with
    | App (lambda1, lambda2) ->
      let (system1, t1) = to_system lambda1 types in
      let (system2, t2) = to_system lambda2 types in
      let new_type = S_Elem (Stream.next new_types_stream) in
      (system1 @ system2 @ [(t1, S_Arrow(t2, new_type))], new_type)
    | Var v -> ([], S_Elem (SM.find v types))
    | Abs (v, l) ->
      let new_map = add_type_to_map types v in
      let (system, t1) = to_system l new_map in
      (system, S_Arrow(S_Elem (SM.find v new_map), t1)) in
  let free = free_vars lambda in
  let types = List.fold_left add_type_to_map SM.empty free in
  let (system, t) = to_system lambda types in
  (* print_string ("We need calculate " ^ (string_of_s_type t) ^ "\n");
  for i = 0 to (List.length system) - 1 do
    let (key, value) = List.nth system i in
    print_string ("\t" ^ (string_of_s_type key) ^ " = " ^ (string_of_s_type value) ^ "\n");
  done; *)
  
  match solve_system (List.map (fun (a, b) -> (term_of_simp_type a, term_of_simp_type b)) system) with
  | None -> None
  | Some solution ->
    let lambda_type_term = apply_substitution solution (term_of_simp_type t) in
    let to_type_list = List.map (fun (a, b) -> (a, simp_type_of_term b)) in
    Some (to_type_list solution, simp_type_of_term lambda_type_term);;

let rec term_of_hm_type hm_type =
  let to_term = term_of_hm_type in
  match hm_type with
  | HM_Elem a  -> Var a
  | HM_Arrow (a, b) -> Fun ("->", [ (to_term a); (to_term b) ])
  | _ -> failwith "Quanifier is not lambda";;

let rec hm_type_of_term term =
  let to_type = hm_type_of_term in
  match term with
  | Var a  -> HM_Elem a
  | Fun (f, [l;r]) when f = "->" -> HM_Arrow(to_type l, to_type r)
  | _ -> failwith "Fun is not arrow";;

let rec free_vars hm_lambda =
  match hm_lambda with
  | HM_Var a -> SS.singleton a
  | HM_App (a, b) -> SS.union (free_vars a) (free_vars b)
  | HM_Abs (a, b) -> SS.remove a (free_vars b)
  | HM_Let (a, b, c) -> let free_vars_c = SS.remove a (free_vars c) in
    SS.union (free_vars b) free_vars_c;;

let rec free_types hm_type =
  match hm_type with
  | HM_Elem a  -> SS.singleton a
  | HM_Arrow (a, b) -> SS.union (free_types a) (free_types b)
  | HM_ForAll (a, b) -> SS.remove a (free_types b);;

let rec apply_type_subst subst hm_type =
  match hm_type with
  | HM_Elem a when SM.mem a subst -> SM.find a subst
  | HM_Elem a -> hm_type
  | HM_Arrow (a, b) ->
    HM_Arrow (apply_type_subst subst a, apply_type_subst subst b)
  | HM_ForAll (a, b) ->
    HM_ForAll (a, apply_type_subst (SM.remove a subst) b);;

let composition subst1 subst2 =
  let for_fold key value source = SM.add key value source in
  SM.fold for_fold (SM.map (apply_type_subst subst1) subst2) subst1;;

let apply_subst_to_types subst = SM.map (apply_type_subst subst);;

let generalize type_env hm_type =
  let add_free_types key value = SS.union (free_types value) in
  let free_env_types = SM.fold add_free_types type_env SS.empty in
  let free_hm_types = free_types hm_type in
  let new_forall_vars = SS.diff free_hm_types free_env_types in
  let add_quantifier var hm_type = HM_ForAll (var, hm_type) in
  SS.fold add_quantifier new_forall_vars hm_type;;

let unique_var = Stream.from (fun i -> Some ("var" ^ string_of_int i));;
let algorithm_w hm_lambda = 
  let rec instantiate hm_type =
  match hm_type with
  | HM_ForAll (a, b) ->
    let subst = SM.singleton a (HM_Elem (Stream.next unique_var)) in
    apply_type_subst subst (instantiate b)
  | _ -> hm_type in

  let rec algorithm_w_rec type_env hm_lambda =
    match hm_lambda with
    | HM_Var a when SM.mem a type_env ->
      (SM.empty, instantiate (SM.find a type_env))
    | HM_Var a -> failwith "Free variables"
    | HM_App (a, b) ->
      (let (s1, t1) = algorithm_w_rec type_env a in
       let (s2, t2) = algorithm_w_rec (apply_subst_to_types s1 type_env) b in
       let new_type = HM_Elem (Stream.next unique_var) in
       let left = apply_type_subst s2 t1 in
       let right = HM_Arrow (t2, new_type) in
       let equation = (term_of_hm_type left, term_of_hm_type right) in
       match solve_system [equation] with
       | None -> failwith "No solution"
       | Some answer ->
         let add_subst (str, term) = SM.add str (hm_type_of_term term) in
         let v = List.fold_right add_subst answer SM.empty in
         let unifier = composition v (composition s2 s1) in
         (unifier, apply_type_subst unifier new_type))
    | HM_Abs (a, b) ->
      let new_type = HM_Elem (Stream.next unique_var) in
      let type_env = SM.add a new_type (SM.remove a type_env) in
      let (s1, t1) = algorithm_w_rec type_env b in
      (s1, HM_Arrow (apply_type_subst s1 new_type, t1))
    | HM_Let (a, b, c) ->
      let (s1, t1) = algorithm_w_rec type_env b in
      let a_type = generalize (apply_subst_to_types s1 type_env) t1 in
      let type_env = apply_subst_to_types s1 (SM.remove a type_env) in
      let type_env = SM.add a a_type type_env in
      let (s2, t2) = algorithm_w_rec type_env c in
      (composition s2 s1, t2)
  in
  let free = free_vars hm_lambda in
  let bound_to_unique v = SM.add v (HM_Elem (Stream.next unique_var)) in
  let type_environment = SS.fold bound_to_unique free SM.empty in
  try
    let (unifier, hm_type) = algorithm_w_rec type_environment hm_lambda in
    Some (SM.bindings unifier, hm_type)
  with _ -> None;;