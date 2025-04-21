# TCP Stack Test Harness

This project provides a test harness for developing a TCP stack from scratch. It includes a client and server program that communicate through TUN interfaces connected via a bridge.

## Prerequisites

- Linux system with root/sudo access
- Go 1.16 or later
- `ip` command-line tool (part of iproute2)

## Setup

1. Make the setup script executable:
   ```bash
   chmod +x setup_bridge.sh
   ```

2. Run the setup script to create the bridge and TUN interfaces:
   ```bash
   sudo ./setup_bridge.sh
   ```

3. Build the client and server programs:
   ```bash
   go build -o bin/client ./cmd/client
   go build -o bin/server ./cmd/server
   ```

## Running the Test

1. In one terminal, run the server:
   ```bash
   sudo ./bin/server
   ```

2. In another terminal, run the client:
   ```bash
   sudo ./bin/client
   ```

The client will send ICMP echo request packets (ping) to the server every second, and the server will display the received packets.

## Network Configuration

- Bridge interface: `br0`
- Server TUN interface: `tun0` (10.0.0.1/24)
- Client TUN interface: `tun1` (10.0.0.2/24)

## Cleanup

To remove the bridge and TUN interfaces:
```bash
sudo ip link delete br0
sudo ip link delete tun0
sudo ip link delete tun1
``` 