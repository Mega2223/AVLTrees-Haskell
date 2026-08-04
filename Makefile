
build:
	mkdir bin || true
	ghc main.hs -Wno-tabs -o bin/main

run:
	make build && bin/main