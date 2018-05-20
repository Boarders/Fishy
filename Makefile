cabal:=cabal new-

.PHONY: build
build:
	${cabal}build && ${cabal}run && open test.svg

.PHONY: repl
repl:
	${cabal}repl
