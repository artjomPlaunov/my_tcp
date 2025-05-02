from scapy.all import *

# Create an ICMP Echo Request packet
pkt = IP(src="10.0.1.5", dst="10.0.0.2")/ICMP(type=8, id=0x1234, seq=1)

# Save raw bytes to a file
with open("packet.bin", "wb") as f:
    f.write(bytes(pkt))