all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build \
	  --flag bugsnag-haskell:examples \
	  --dependencies-only --test --no-run-tests
	stack install --copy-compiler-tool \
	  brittany \
	  fast-tags \
	  hlint \
	  stylish-haskell \
	  weeder

.PHONY: build
build:
	stack build \
	  --coverage \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build \
	  --coverage \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test

.PHONY: lint
lint:
	stack exec hlint .
	stack exec weeder .

.PHONY: clean
clean:
	stack clean
