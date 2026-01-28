PHONY: dev build publish

dev:
	NODE_OPTIONS=--openssl-legacy-provider elm-app start

build-app:
	NODE_OPTIONS=--openssl-legacy-provider elm-app build

publish:
	gh-pages -d build
