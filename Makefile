all: hw1 hw1_reduction hw2_unify

hw1: hw1.native
hw1_reduction: hw1_reduction.native
hw2_unify: hw2_unify.native

%.native:
	@echo "Building <$*>:"
	@ocamlbuild -use-ocamlfind $@
	@mkdir -p ./_build/bin/
	@mv $@ ./_build/bin/$*

clean:
	rm -rf _build
.PHONY: all
