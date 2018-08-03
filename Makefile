all: setup build test lint

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	stack build $(STACK_ARGUMENTS) \
	  --flag bugsnag-haskell:examples \
	  --dependencies-only --test --no-run-tests
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) \
	  --coverage \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test --no-run-tests

.PHONY: watch
watch:
	stack build $(STACK_ARGUMENTS) \
	  --coverage \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test --file-watch

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) \
	  --coverage \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test

.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint src test
	stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: clean
clean:
	stack clean
