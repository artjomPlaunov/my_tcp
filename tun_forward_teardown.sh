sysctl -w net.ipv4.ip_forward=0
firewall-cmd --zone=trusted --remove-interface=tun0
firewall-cmd --zone=trusted --remove-interface=tun1
firewall-cmd --permanent --zone=trusted --remove-interface=tun0
firewall-cmd --permanent --zone=trusted --remove-interface=tun1
firewall-cmd --reload
