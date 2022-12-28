run:
	cabal run

run-external:
	cabal run fakeworld -- -e

.PHONY: run run-external
