.PHONY: all build clean run

all: build

build:
	sudo iptables -t raw -D PREROUTING 1 || true
	sudo iptables -t raw -D PREROUTING 1 || true
	sudo iptables -t raw -I PREROUTING -i tun0 -p tcp -j NOTRACK
	sudo iptables -t raw -I PREROUTING -i tun1 -p tcp -j NOTRACK
	sudo ./tun_forward.sh
	dune build

clean:
	sudo ./tun_forward_teardown.sh
	sudo iptables -t raw -D PREROUTING 1 || true
	sudo iptables -t raw -D PREROUTING 1 || true
	sudo env PATH=$$PATH dune clean

preroute_rules:
	sudo iptables -t raw -I PREROUTING -i tun0 -p tcp -j NOTRACK
	sudo iptables -t raw -I PREROUTING -i tun1 -p tcp -j NOTRACK
	sudo iptables -t raw -L -v -n --line-numbers

view_preroute_rules:
	sudo iptables -t raw -L -v -n --line-numbers

run:
	sudo env PATH=$$PATH dune exec ./main.exe tun0 10.0.0.1/24

run-peer-test:
	sudo env PATH=$$PATH dune exec ./peer_test.exe tun1 10.0.1.1/24 