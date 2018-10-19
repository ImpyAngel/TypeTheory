open Hw1;;

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type
let infer_simp_type = failwith "Not implemented";;

let string_of_s_type = failwith "Not implemented";;

type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type

let algorithm_w hm_lambda = failwith "Not implemented";;

let string_of_hm_type = failwith "Not implemented";;