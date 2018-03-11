all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build \
	  --flag bugsnag-reporter:examples \
	  --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build \
	  --flag bugsnag-reporter:examples \
	  --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build \
	  --flag bugsnag-reporter:examples \
	  --fast --pedantic --test

.PHONY: lint
lint:
	hlint .
	weeder .

.PHONY: clean
clean:
	stack clean
