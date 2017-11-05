.PHONY: build
.DEFAULT_GOAL := devel

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: gulp-build
	elm make ./src/Main.elm \
		--output ./dist/assets/javascripts/elm.js \
		--warn

gulp-build:
	gulp build

devel:
	elm-live \
		./src/Main.elm \
		--output ./dist/assets/javascripts/elm.js \
		--warn \
		--port=3000 \
		--dir=./dist \
		--before-build=gulp \
		--open
