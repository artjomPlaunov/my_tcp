sudo sysctl -w net.ipv4.ip_forward=1
sudo firewall-cmd --zone=trusted --add-interface=tun0
sudo firewall-cmd --zone=trusted --add-interface=tun1
sudo firewall-cmd --permanent --zone=trusted --add-interface=tun0
sudo firewall-cmd --permanent --zone=trusted --add-interface=tun1
sudo firewall-cmd --reload
