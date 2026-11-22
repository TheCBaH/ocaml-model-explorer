
default: build

build:
	opam exec -- dune $@

runtest:
	opam exec -- dune $@ --auto-promote

static:
	opam exec -- dune build --profile static

format:
	opam exec dune fmt

run:
	opam exec dune exec ./main.exe

top:
	opam exec dune exec ./example_top.exe

utop:
	opam exec dune utop

clean:
	opam exec dune $@

playwright-check:
	${CURDIR}/scripts/playwright-check.sh

namespace-check:
	d=$$(mktemp -d) && \
	  opam exec -- dune build -p model_explorer --build-dir "$$d/_build" @install && \
	  opam exec -- dune install --build-dir "$$d/_build" --prefix "$$d" model_explorer && \
	  opam exec -- sh scripts/namespace-check.sh --fail --prefix Model_explorer \
	    --expect-libs scripts/namespace.expect "install=$$d/lib/model_explorer"; rc=$$?; \
	rm -rf "$$d"; exit $$rc

.PHONY: default clean format run top utop playwright-check namespace-check

server.model-explorer.js:
	cd model-explorer/src/custom_element_demos/vanilla_js; ./build_and_deploy.sh

server.model-explorer.ts:
	cd model-explorer/src/custom_element_demos/vanilla_ts; npm run build_and_deploy

server.stop:
	${CURDIR}/scripts/server.sh stop
