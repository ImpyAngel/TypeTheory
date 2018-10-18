all: hw1 hw1_reduction

hw1: hw1.native
hw1_reduction: hw1_reduction.native

%.native:
	@echo "Building <$*>:"
	@ocamlbuild -use-ocamlfind $@
	@mkdir -p ./_build/bin/
	@mv $@ ./_build/bin/$*

clean:
	rm -rf _build
.PHONY: all
