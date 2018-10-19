(* created by ikefir https://github.com/iKefir/OCamlHW *)
open Hw1;;
open Hw1_reduction;;
open Hw2_unify;;
(* open Hw2_inference;; *)

type my_type =
    | Header of string * (my_type list)
    | T_case of string * (my_type list)
    | String of string
    | String_list of string list
    | Bool of bool
    | Int of int
    | Int_list of int list
    | Lambda of lambda
    | Term of algebraic_term
    | Eq of (algebraic_term * algebraic_term)
    | System of (algebraic_term * algebraic_term) list
    | Solved_system of (string * algebraic_term) list option
    (* | S_type of simp_type *)
    (* | HM_type of hm_type *)

let rec print_string_list = function
    | [] -> ()
    | head::tail -> print_string head ; print_string " " ; print_string_list tail;;

let rec print_int_list = function
    | [] -> ()
    | head::tail -> print_int head ; print_string " " ; print_int_list tail;;

let rec string_of_term t = match t with
    | Var a -> a
    | Fun (f, lst) -> f ^ "(" ^ (String.concat ", " (List.map (string_of_term) lst)) ^ ")";;

let string_of_eq (t1, t2) = string_of_term t1 ^ " = " ^ string_of_term t2;;


let string_of_system s = String.concat "\n" (List.map string_of_eq s);;


let print_bool bl = print_string(string_of_bool bl);;

let print_lambda lambda = print_string(string_of_lambda lambda);;

let print_term term = print_string (string_of_term term);;

let print_eq eq = print_string (string_of_eq eq);;

(* let print_s_type s_t = print_string(string_of_s_type s_t);; *)

(* let print_hm_type hm_t = print_string(string_of_hm_type hm_t);; *)

let tab_size = ref 0;;

let rec my_print obj =
    let tabs() = for i = 1 to !tab_size do print_string("    ") done in

    let print_obj obj = (match obj with
            | Header (v, other) -> print_string (v ^ "\n"); tab_size := !tab_size + 1; List.iter my_print other; tab_size := !tab_size - 1
            | T_case (v, other) -> print_string (v ^ "\n"); List.iter my_print other
            | String v -> print_string v
            | String_list v -> print_string_list v
            | Bool v -> print_bool v
            | Int v -> print_int v
            | Int_list v -> print_int_list v
            | Lambda v -> print_lambda v
            | Term v -> print_term v
            | Eq (v1, v2) -> print_eq (v1, v2)
            | System v -> List.iter (fun eq -> my_print (Eq eq)) v
            | Solved_system v -> (match v with
                | None -> my_print(String "No solution, sorry(")
                | Some sys -> List.iter (fun (var, term) -> my_print (String (var ^ " = " ^ (string_of_term term)))) sys)
            (* | S_type v -> print_s_type v *)
            (* | HM_type v -> print_hm_type v *)
            ) in

    (match obj with
        | System _ | Solved_system _ -> ()
        | _ -> tabs());

    print_obj obj;
    print_string("\n");;


let first_hw_tests = Header ("------FIRST HW----", [
    Header ("----FIRST PART----", [
        T_case ("int_of_peano",
            [Int (int_of_peano (S (S (Z))))]);

        T_case ("inc",
            [Int (int_of_peano (inc (peano_of_int 2)))]);

        T_case ("add",
            [Int (int_of_peano (add (peano_of_int 2) (peano_of_int 5)))]);

        T_case ("sub",
            [Int (int_of_peano (sub (peano_of_int 5) (peano_of_int 2)));
            Int (int_of_peano (sub (peano_of_int 2) (peano_of_int 5)))]);

        T_case ("mul",
            [Int (int_of_peano (mul (peano_of_int 5) (peano_of_int 2)))]);

        T_case ("power",
            [Int (int_of_peano (power (peano_of_int 15) (peano_of_int 2)));
            Int (int_of_peano (power (peano_of_int 2) (peano_of_int 5)))])
    ]);

    Header ("---SECOND PART----", [
        T_case ("rev",
            [Int_list (rev ([6; 7]));
            Int_list (rev (rev ([6; 7])))]);

        T_case ("merge_sort",
            [Int_list (merge_sort ([6; 7]));
            Int_list (merge_sort ([6; 7; 1; 2; 3; 19]))]);
    ]);

    Header ("----THIRD PART----", [

        T_case ("string of lambda",
            [String (string_of_lambda (App ((Var "b"), (Abs ("x", Abs ("y", Var "a"))))))]);

        T_case ("lambda of string",
            [Lambda (lambda_of_string "\\x.\\y.x");
            Lambda (lambda_of_string "b \\x.\\y.a");
            Lambda (lambda_of_string ("a b c d e f g"));
            String "b \\x.\\y.a";
            Lambda (lambda_of_string "b \\x.\\y.a");
            Lambda(lambda_of_string (string_of_lambda (lambda_of_string "b \\x.\\y.a")));
            Lambda(lambda_of_string (string_of_lambda (lambda_of_string "(b (\\x.(\\y.a)))")))])
    ])
]);;

let second_hw_tests = Header ("-----SECOND HW----", [
    T_case ("free_to_subst",
        [String "a in x instead x";
        Bool (free_to_subst (lambda_of_string "a") (lambda_of_string "x") "x");
        String ("a in b instead x");
        Bool (free_to_subst (lambda_of_string "a") (lambda_of_string "b") "x");
        String ("\\x.\\y.x in \\x.\\y.x instead x");
        Bool (free_to_subst (lambda_of_string "\\x.\\y.x") (lambda_of_string "\\x.\\y.x") "x");
        String ("\\x.\\y.x in (\\x.\\y.x) x instead x");
        Bool (free_to_subst (lambda_of_string "\\x.\\y.x") (lambda_of_string "(\\x.\\y.x) x") "x");
        String ("\\a.\\b.a in \\x.\\y.x instead x");
        Bool (free_to_subst (lambda_of_string "\\a.\\b.a") (lambda_of_string "\\x.\\y.x") "x");
        String ("x in (\\x.\\y.v) x instead v");
        Bool (free_to_subst (lambda_of_string "x") (lambda_of_string "(\\x.\\y.v) x") "v");
        String ("\\y.z in \\x.v instead v");
        Bool (free_to_subst (lambda_of_string "\\y.z") (lambda_of_string "\\x.v") "v")]);

    T_case ("free_vars",
        [String_list (free_vars (lambda_of_string ("\\x.\\y.x y")));
        String_list (free_vars (lambda_of_string ("\\x.\\y.x y z")));
        String_list (free_vars (lambda_of_string ("\\x.\\y.z \\z.y r")))]);

    T_case ("is_alpha_equivalent",
        [Bool (is_alpha_equivalent (lambda_of_string "\\x.\\y.x") (lambda_of_string "\\x.\\y.x"));
        Bool (is_alpha_equivalent (lambda_of_string "\\y.\\x.y") (lambda_of_string "\\x.\\y.x"));
        Bool (is_alpha_equivalent (lambda_of_string "y x") (lambda_of_string "x y"));
        Bool (is_alpha_equivalent (lambda_of_string "(\\x.\\y.\\x.x) a") (lambda_of_string "(\\x.\\y.\\z.x) a"));
        Bool (is_alpha_equivalent (lambda_of_string "\\x.\\subst_arg0.x subst_arg0") (lambda_of_string "\\y.\\subst_arg0.y subst_arg0"));
        Bool (is_alpha_equivalent (lambda_of_string "\\x.\\y.x") (lambda_of_string "b \\x.\\y.x"));
        Bool (is_alpha_equivalent (lambda_of_string "(var_name2 (\\z.var_name1))") (lambda_of_string "(z (\\z.y))"))]);

    T_case ("is_normal_form",
        [Bool (is_normal_form (lambda_of_string "a"));
        Bool (is_normal_form (lambda_of_string "a b c"));
        Bool (is_normal_form (lambda_of_string "\\x.x"));
        Bool (is_normal_form (lambda_of_string "(\\x.x) b"));
        Bool (is_normal_form (lambda_of_string "a (\\x.x) b c d"));
        Bool (is_normal_form (lambda_of_string "a \\x.x b c d"))]);

    T_case ("normal_beta_reduction",
        [Lambda (normal_beta_reduction (lambda_of_string "a"));
        Lambda (normal_beta_reduction (lambda_of_string "(\\x.x) a"));
        Lambda (normal_beta_reduction (lambda_of_string "(\\x.y) a"));
        Lambda (normal_beta_reduction (lambda_of_string "(\\x.\\y.x) y"));
        Lambda (normal_beta_reduction (lambda_of_string "(\\x.x \\z.x) z"));
        Lambda (normal_beta_reduction (lambda_of_string "(\\p.\\x.\\x.x p) a"));
        Lambda (normal_beta_reduction (lambda_of_string "(\\p.\\x.\\var_name1.x p) x"));
        Lambda (normal_beta_reduction (lambda_of_string "((\\x.\\y.x) (\\z.y)) k"))]);

    T_case ("free_vars",
        [String_list (free_vars (lambda_of_string ("(\\x.\\y.x) y")))]);

    T_case ("reduce_to_normal_form",
        [Lambda (reduce_to_normal_form (lambda_of_string "(\\x.\\y.z x (\\u.u x)) (\\x.w x)"));
        Lambda (reduce_to_normal_form (lambda_of_string "(\\x.x \\z.x) z"));
        Lambda (reduce_to_normal_form (lambda_of_string "(\\x.x (t)) (\\x.x (t))"));
        Lambda (reduce_to_normal_form (lambda_of_string "((\\z.(\\y.y)) ((\\x.x x) (\\x.x x))) t"))])
]);; 

let third_hw_tests = Header ("------THIRD HW----", [
    T_case ("system_to_equation",
        let eq1 = (Var "a", Var "b") in
        let eq2 = (Fun ("r", [Var "a"]), Fun ("g", [Var "b"])) in
        let eq3 = (Fun ("fun_name1", [Fun ("fun_name2", [Fun ("fun_name3", [Var "kk"])])]), Fun ("fun_name4", [Fun ("fun_name5", [Fun ("fun_name6", [Var "gg"])])])) in
        let sys = [system_to_equation [system_to_equation [eq1]]] in
            [Eq (system_to_equation [eq1; eq2]);
            Eq (system_to_equation [eq1; eq2; eq3]);
            Eq (system_to_equation sys)]);

    T_case ("apply_substitution",
        let subst = [("a", Var "kek"); ("b", Fun ("very_smart_function", [Var "kek"; Var "kek"; Var "kek"]))] in
            [Term (apply_substitution subst (Var "a"));
            Term (apply_substitution subst (Var "b"));
            Term (apply_substitution subst (Var "c"))]);

    T_case ("check_solution",
        let subst = [("a", Var "kek"); ("b", Fun ("very_smart_function", [Var "kek"; Var "kek"; Var "kek"]))] in
        let smart_sys = [(Var "kek", Var "a")] in
        let smarter_sys = [(Var "b", Fun ("very_smart_function", [Var "kek"; Var "kek"; Var "kek"]))] in
        let smart_but_wrong_sys = [(Var "r", Fun ("r", [Var "r"]));(Fun ("e", [Var "a"]), Fun ("e", [Var "a"; Var "b"]))] in
            [Bool (check_solution subst smart_sys);
            Bool (check_solution subst smarter_sys);
            Bool (check_solution subst smart_but_wrong_sys)]);

    T_case ("solve_system",
        let eq1 = (Fun("A", [Var "b"]), Var "a") in
        let eq2 = (Fun("B", [Var "c"; Var "a"]), Fun("B", [Fun("C", []); Var "a"])) in
        let eq3 = (Fun ("fun", [Var "a"]), Fun ("fun", [Var "f"])) in
        let eq4 = (Var "d", Fun("other", [Var "e"])) in
        let sys = [eq1; eq1; eq2; eq3; eq4] in
        let res = solve_system sys in
        let check_correctness sys res = match res with
                | None -> false
                | Some subst -> check_solution subst sys in
            [Solved_system res;
            Bool (check_correctness sys res)]
            )
]);;

(* let fourth_hw_tests = Header ("-----FOURTH HW----", [
    T_case ("infer_simp_type",
        let lambda1 = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x")))) in

        let check_infer_type lambda = match infer_simp_type lambda with
            | None -> String "Failed to infer"
            | Some (l, res) -> T_case ("check_type_inference:",
                                  [String "Substs:"] @
                                  List.map (fun (key, s_t) -> String (key ^ " = " ^ (string_of_s_type s_t))) l @
                                  [String ("Inferred type:");
                                  S_type res]) in

            [check_infer_type lambda1;
            check_infer_type (lambda_of_string "\\a.\\b.\\c.\\d.\\e.\\f.\\g.(a b c d e f g)")]);

    T_case ("algorithm_w",
        let l1 = HM_Abs ("x", HM_Var "x") in
        let l2 = HM_Let ("q", HM_App (HM_Var "f", HM_App (HM_Var "f", HM_Var "x")), HM_Abs ("f", HM_Abs ("x", HM_Var "q"))) in

        let check_algorithm_w lambda = match algorithm_w lambda with
            | None -> String "Failed to infer"
            | Some (l, res) -> T_case ("check_algorithm_w:",
                                  [String "Substs:"] @
                                  List.map (fun (key, s_t) -> String (key ^ " = " ^ (string_of_hm_type s_t))) l @
                                  [String ("Inferred type:");
                                  HM_type res]) in
        [check_algorithm_w l1;
        check_algorithm_w l2;
        check_algorithm_w (HM_Var "x")]
        )
]);; *)

my_print first_hw_tests;;
my_print second_hw_tests;;
my_print third_hw_tests;;
(* my_print fourth_hw_tests;; *)

(* my_print (Lambda (reduce_to_normal_form (lambda_of_string "(\\x.\\y.x) (\\y.y)"))) *) 