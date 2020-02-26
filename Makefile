all: setup build test lint

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	# Avoid ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)
	stack build $(STACK_ARGUMENTS) -j 1 Cabal haskell-src-exts
	stack build $(STACK_ARGUMENTS) \
	  --coverage \
	  --flag bugsnag-haskell:examples \
	  --fast --test --no-run-tests --dependencies-only

.PHONY: setup.lint
setup.lint:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool hlint weeder
	if [ ! -f ~/.hlint.yaml ]; then \
	  curl -L -o .hlint.yaml https://raw.githubusercontent.com/pbrisbin/dotfiles/master/hlint.yaml; \
	if

.PHONY: setup.coverage
setup.coverage:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool cc-coverage
	curl -L -o cc-test-reporter https://codeclimate.com/downloads/test-reporter/test-reporter-latest-linux-amd64
	chmod +x ./cc-test-reporter

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
	stack build $(STACK_ARGUMENTS) $(STACK_TEST_TARGET) \
	  --coverage \
	  --flag bugsnag-haskell:examples \
	  --fast --pedantic --test

.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint src test
	stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: coverage.upload
coverage.upload:
	stack exec $(STACK_ARGUMENTS) tix2cc | ./cc-test-reporter upload-coverage --input -

.PHONY: clean
clean:
	stack clean
