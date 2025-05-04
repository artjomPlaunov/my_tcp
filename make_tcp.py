from scapy.all import *

# Create a TCP packet (no IP header)
tcp = TCP(sport=12345, dport=9999, flags="S", seq=1)
#tcp = ICMP(type=8, id=0x1234, seq=1)

# Convert the TCP layer to raw bytes
raw_tcp = bytes(tcp)

# Write the raw TCP packet (just the TCP layer) to a file
with open("tcp.bin", "wb") as f:
    f.write(raw_tcp)
