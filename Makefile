.PHONY: all build clean run

all: build

preroute: 
	iptables -t raw -I PREROUTING -i tun0 -p tcp -j NOTRACK
	iptables -t raw -I PREROUTING -i tun1 -p tcp -j NOTRACK

clean_preroute:
	iptables -t raw -D PREROUTING 1 || true
	iptables -t raw -D PREROUTING 1 || true

forwarding: 
	sysctl -w net.ipv4.ip_forward=1
	firewall-cmd --zone=trusted --add-interface=tun0
	firewall-cmd --zone=trusted --add-interface=tun1
	firewall-cmd --permanent --zone=trusted --add-interface=tun0
	firewall-cmd --permanent --zone=trusted --add-interface=tun1
	firewall-cmd --reload

clean_forwarding:
	sysctl -w net.ipv4.ip_forward=0
	firewall-cmd --zone=trusted --remove-interface=tun0 || true
	firewall-cmd --zone=trusted --remove-interface=tun1 || true
	firewall-cmd --permanent --zone=trusted --remove-interface=tun0 || true
	firewall-cmd --permanent --zone=trusted --remove-interface=tun1 || true
	firewall-cmd --reload

network:
	$(MAKE) clean_preroute
	$(MAKE) preroute
	$(MAKE) forwarding

clean_network:
	$(MAKE) clean_preroute
	$(MAKE) clean_forwarding

build:
	sudo $(MAKE) network
	sudo env PATH==$$PATH dune build

dune-build:
	sudo env PATH==$$PATH dune build

dune-clean:
	sudo env PATH==$$PATH dune clean

dune-fmt:
	sudo env PATH==$$PATH dune fmt

clean:
	sudo $(MAKE) clean_network
	sudo env PATH==$$PATH dune clean

view_network:
	ip link
	ip route
	sudo iptables -t raw -L -v -n --line-numbers

run-host:
	sudo env PATH=$$PATH dune exec ./host.exe tun0 10.0.0.1/24 10.0.1.5

trace-host: 
	sudo env PATH=$$PATH eio-trace run -- ./_build/default/host.exe tun0 10.0.0.1/24 10.0.1.5

run-peer:
	sudo env PATH=$$PATH dune exec ./peer.exe tun1 10.0.1.1/24 10.0.0.7