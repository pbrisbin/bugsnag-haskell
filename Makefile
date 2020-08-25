all: setup setup.lint build test lint

coverage: setup setup.coverage build.coverage test.coverage coverage.upload

.PHONY: setup
setup:
	stack setup
	stack build \
	  --flag bugsnag-haskell:examples \
	  --fast --test --no-run-tests --dependencies-only

.PHONY: setup.lint
setup.lint:
	stack install --copy-compiler-tool hlint weeder

.PHONY: setup.coverage
setup.coverage:
	stack install --copy-compiler-tool hpc-lcov
	curl -L https://codeclimate.com/downloads/test-reporter/test-reporter-latest-linux-amd64 > bin/cc-test-reporter
	chmod +x bin/cc-test-reporter

.PHONY: setup.tools
setup.tools:
	stack install --copy-compiler-tool brittany stylish-haskell fast-tags

.PHONY: build
build:
	stack build \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test --no-run-tests

.PHONY: build.coverage
build.coverage:
	stack build \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test --no-run-tests \
	  --coverage

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

.PHONY: test.coverage
test.coverage:
	stack build \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test \
	  --coverage

.PHONY: lint
lint:
	stack exec hlint src test
	stack exec weeder .

.PHONY: coverage.upload
coverage.upload:
	stack exec -- hpc-lcov --output coverage/lcov.info
	bin/cc-test-reporter after-build

.PHONY: clean
clean:
	stack clean
