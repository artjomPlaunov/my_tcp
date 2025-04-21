.PHONY: all build clean run

all: build

build:
	dune build

clean:
	sudo env PATH=$$PATH dune clean

run:
	sudo env PATH=$$PATH dune exec ./main.exe