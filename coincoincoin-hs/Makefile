.DEFAULT_GOAL := help

IMAGE_NAME=coincoincoin/coincoincoin
BASE_IMAGE_NAME=coincoincoin/base

.PHONY: help
help: ## Helps
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: setup
setup: ## Installs project dependencies
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: devel
devel: ## Continuously builds/test on file change
	stack test --file-watch --exec="$(MAKE) lint"

.PHONY: test
test: ## Builds, tests, lints
	stack test --exec="$(MAKE) lint"

.PHONY: stylish-haskell
stylish-haskell: ## Runs a Haskell auto-formatter across source code directories
	find ./*/test -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i
	find ./*/src -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i
	find ./*/app -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i

.PHONY: lint
lint:
	hlint .

image: ## Builds a new image containing all project executables
	stack image container --build && \
		docker tag \
			$(BASE_IMAGE_NAME):latest \
			$(IMAGE_NAME)
