type algebraic_term = Var of string | Fun of string * (algebraic_term list)

module SS = Set.Make(String);;

let rec term_names term = match term with
 	 | Var v -> SS.singleton v
 	 | Fun (name, terms) -> List.fold_left (fun set v -> SS.union (term_names v) set) (SS.singleton name) terms

let all_names_lst lst = List.fold_left (fun set v -> SS.union (term_names v) set) (SS.empty) lst
 	
let system_to_equation x = 
 	let (first, second) = List.split x in	
 	let all_names = SS.union (all_names_lst first) (all_names_lst second) in
 	let new_var = "fun" ^ (SS.max_elt all_names) in 
     (Fun (new_var, first), Fun (new_var, second));;

(* x - subs, y - term *)
let apply_substitution x y =
	let rec one_substitution kterm source = 
	let (key, new_term) = kterm in
	match source with
		| Var var -> if var = key then new_term else source
		| Fun (name, terms) -> Fun (name, List.map (one_substitution kterm) terms) in 
	List.fold_right one_substitution x y

let check_solution x y = 
  	let (l, r) = system_to_equation y in
  	let substitute = apply_substitution x in
  	substitute l = substitute r;;

let solve_system x = 
	let rec rec_solve_system system_list solution = match system_list with
		| [] -> solution
	 	| x::xs -> 
			let subs_for_system v term = 
				let apsuvt = apply_substitution [(v, term)] in
				List.map (fun (first, second) -> (apsuvt first, apsuvt second)) xs in
			let check_var v term = if (SS.mem v (term_names term))
				then failwith "x = f(..x..)"
				else rec_solve_system (subs_for_system v term) (solution @ [(v, term)]) in
	 	match x with
			| (l, r) when l = r -> rec_solve_system xs solution
			| (Var v, term) -> check_var v term
			| (term, Var v) -> check_var v term
			| (Fun (v1, terms1), Fun (v2, terms2)) -> if (v1 = v2) && ((List.length terms1) = (List.length terms2)) 
				then rec_solve_system (xs @ List.map2 (fun first second -> (first, second)) terms1 terms2) solution 
				else failwith "f(..) = g(...)" in
	try 
		Some (rec_solve_system x [])
	with _ -> None