cd src/ 
ocamlc hw1.mli hw1_reduction.mli hw2_unify.mli hw2_inference.mli
ocamlc -pp "camlp4o pa_extend.cmo" -I +camlp4 hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw2_unify.mli hw2_unify.ml hw2_inference.mli hw2_inference.ml test.ml
./a.out