.PHONY: all build clean run

all: build

build:
	dune build

clean:
	sudo env PATH=$$PATH dune clean
	sudo ip link delete tun0

run:
	sudo env PATH=$$PATH dune exec ./main.exe