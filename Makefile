.PHONY: tape, run, tests, repl

tape:
ifndef TAPE
	$(error TAPE is undefined; use like 'make tape TAPE="replicate 10 0"')
endif
	@sed -i 's/^input = .*/input = $(TAPE)/' scripts/GenTape.hs
	@cabal run mktape
	@echo "------"
	@cat app/TF/Generated/Tape.hs
	@echo "\n------"

run:
	@cabal run app -v0

repl:
	@cabal repl app

tests:
	@if command -v bun >/dev/null 2>&1; then \
		bun spec/run-tests.mjs; \
	else \
		node spec/run-tests.mjs; \
	fi
