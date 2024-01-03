.DEFAULT_GOAL := all

.PHONY: all
all: build

.PHONY: install-deps
install-deps: install-switch 
	opam install -y ocamlformat=0.26.1 ocaml-lsp-server
	opam install -y --deps-only --with-test --with-doc .

.PHONY: install-switch
install-switch:
	opam switch create . 4.14.1

.PHONY: build
build:
	opam exec -- dune build

.PHONY: install
install: all 
	opam exec -- dune install --root .

.PHONY: test
test:
	opam exec -- dune runtest

.PHONY: clean
clean:
	opam exec -- dune clean

.PHONY: doc
doc:
	opam exec -- dune build @doc

.PHONY: fmt
fmt:
	opam exec -- dune fmt

.PHONY: watch
watch:
	opam exec -- dune build @run -w --force --no-buffer

.PHONY: utop
utop:
	opam exec -- dune utop