fmt:
	@dune build --auto-promote @fmt

build: fmt
	@dune build

run:
	@dune exec toposim

clean:
	@dune clean

WATCH ?= @all
watch:
	@dune build $(WATCH) -w

# DO NOT RUN
env:
	eval (opam env)
