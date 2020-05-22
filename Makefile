all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build \
	  --coverage \
	  --flag bugsnag-haskell:examples \
	  --fast --test --no-run-tests --dependencies-only
	stack install --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test --no-run-tests

.PHONY: watch
watch:
	stack build \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test --file-watch

.PHONY: test
test:
	stack build \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test

.PHONY: lint
lint:
	stack exec hlint src test
	stack exec weeder .

.PHONY: clean
clean:
	stack clean
