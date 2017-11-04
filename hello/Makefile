.PHONY: build
.DEFAULT_GOAL := devel

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build:
	elm make ./src/Main.elm \
		--output ./dist/assets/javascripts/main.js \
		--warn

devel:
	elm-live \
		./src/Main.elm \
		--output ./dist/assets/javascripts/main.js \
		--warn \
		--port=3000 \
		--dir=./dist \
		--open
