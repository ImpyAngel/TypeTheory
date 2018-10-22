open Genlex
type peano = Z | S of peano;;

let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l;;

let rec peano_of_int p =
	if p <= 0 then Z else S (peano_of_int (p - 1));;

let rec int_of_peano p = match p with
    Z -> 0
  | S x -> 1 + int_of_peano x;;

let inc x = S x
let rec add x y = match x with
	Z -> y
  | S z -> inc (add z y);;

let rec sub x y = match x, y with
| xx, Z -> xx
| Z, _ -> Z
| S xx, S yy -> sub xx yy;;

let rec mul x y = match x, y with
| _, Z -> Z
| xx, S yy -> add xx (mul xx yy);;

let rec power x y = match x, y with
| xx, Z -> xx
| xx, S yy -> mul xx (power xx yy);;

let rec rev x = match x with
| [] -> []
| (n::ns) -> (rev ns)@[n];;

let rec merge_sort x =
match x with
	| [] -> []
	| (a::[]) -> [a]
	| l -> let rec mergehelper f s = match f, s with
			    	| first, [] -> first
			    	| [], second -> second
			    	| (z::zs) as zzs, ((y::ys) as yys) -> if (z < y)
			    		then z :: (mergehelper zs yys)
			    		else y :: (mergehelper zzs ys) in

			let rec splits l_old acc1 acc2 =
			match l_old with
				| h1::h2::t -> splits t (h1::acc1) (h2::acc2)
				| h1::[] -> ((h1::acc1), acc2)
				| [] -> (acc1, acc2) in

		    let (prev, next) = splits l [] [] in
			mergehelper (merge_sort prev) (merge_sort next)

type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec string_of_lambda x =
	match x with
			Var v -> v
			| Abs (v, x) -> "(" ^ "\\" ^ v ^ "." ^ (string_of_lambda x) ^ ")"
			| App (x, y) -> "(" ^ (string_of_lambda x) ^ " " ^ (string_of_lambda y) ^ ")";;

let lambda_of_string x =
let lexer = make_lexer ["\\"; "."; "("; ")"] in
let rec list_to_app list = match list with
 | x::xs -> list_to_app_acc xs x
 | _ -> failwith "unreachable statement"
and list_to_app_acc list acc = match list with
 | second::tail -> list_to_app_acc tail (App (acc, second))
 | [] -> acc
and parse_lambda = parser
 | [< l = parse_one; tail = lambda_helper l >] -> list_to_app tail
and lambda_helper prev = parser
 | [< next = parse_one; tail = lambda_helper next >] -> (prev::tail)
 | [< >] -> [prev]
and parse_one = parser
 | [< 'Kwd "\\"; 'Ident var; 'Kwd "."; l = parse_lambda >] -> Abs (var, l)
 | [< 'Kwd "("; l = parse_lambda; 'Kwd ")" >] -> l
 | [< 'Ident var >] -> Var var in
parse_lambda(lexer (Stream.of_string x));;
