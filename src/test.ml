(* created by ikefir https://github.com/iKefir/OCamlHW *)
open Hw1;; 
open Hw1_reduction;;

type my_type =
    | Header of string * (my_type list)
    | T_case of string * (my_type list)
    | String of string
    | String_list of string list
    | Bool of bool
    | Int of int
    | Int_list of int list
    | Lambda of lambda

let print_bool bl = print_string(string_of_bool bl);;

let print_lambda lambda = print_string(string_of_lambda lambda);;

let print_string_list list = print_string("lissst");; 

let print_int_list list = print_string("lisintsst");; 
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
            ) in

    (match obj with
        | _ -> tabs());

    print_obj obj;
    print_string("\n");;


(* let first_hw_tests = Header ("------FIRST HW----", [String("s") *)
   (*  Header ("----FIRST PART----", [
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
    ]); *)

(*     Header ("---SECOND PART----", [
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
    ]) *)
(* ]);; *)

(* let second_hw_tests = Header ("-----SECOND HW----", [
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
        Lambda (reduce_to_normal_form (lambda_of_string "(\\x.x \\x.\\x.x \\x.x \\x.w x \\x.x) (\\x.x)"))])
]);;  *)

my_print (Lambda (reduce_to_normal_form (lambda_of_string "(\\x.\\y.x) (\\y.y)")))