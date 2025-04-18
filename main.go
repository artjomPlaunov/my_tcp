// main.go
package main

import (
	"encoding/hex"
	"fmt"
	"net"
	"os"
	"os/exec"
	"syscall"
	"unsafe"
)

type Tun struct {
	name string
	fd   int
	ip   net.IP
	cidr net.IPNet
}

func NewTun(name, cidr string) (*Tun, error) {
	fd, err := createTun(name)
	if err != nil {
		return nil, err
	}

	ip, ipNet, err := net.ParseCIDR(cidr)
	if err != nil {
		return nil, err
	}
	tun := &Tun{name: name, fd: fd, ip: ip, cidr: *ipNet}

	_, err = exec.Command("ip", "addr", "add", cidr, "dev", name).CombinedOutput()
	if err != nil {
		return nil, err
	}

	_, err = exec.Command("ip", "link", "set", name, "up").CombinedOutput()
	if err != nil {
		return nil, err
	}

	return tun, nil
}

func (t *Tun) Read(b []byte) (n int, err error) {
	return syscall.Read(t.fd, b)
}

func (t *Tun) Write(b []byte) (n int, err error) {
	return syscall.Write(t.fd, b)
}

func (t *Tun) Close() error {
	return syscall.Close(t.fd)
}

func (t *Tun) IP() net.IP {
	return t.ip
}

func (t *Tun) CIDR() net.IPNet {
	return t.cidr
}

func createTun(name string) (fd int, err error) {
	fmt.Printf("creating tun %s\n", name)
	flags := syscall.IFF_TUN | syscall.IFF_NO_PI
	fd, err = syscall.Open("/dev/net/tun", syscall.O_RDWR, 0)
	if err != nil {
		return
	}
	var ifr struct {
		name  [16]byte // device name
		flags uint16   // flag
		_     [22]byte //padding
	}
	copy(ifr.name[:], name)
	ifr.flags = uint16(flags)
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, uintptr(fd), syscall.TUNSETIFF, uintptr(unsafe.Pointer(&ifr)))
	if errno != 0 {
		syscall.Close(fd)
		err = errno
		return
	}
	return
}

func main() {
	name0 := "tun0"
	cidr0 := "11.0.0.1/24"
	tun0, err := NewTun(name0, cidr0)
	if err != nil {
		os.Exit(1)
	}
	b0 := make([]byte, 1500)
	for {
		n, _ := tun0.Read(b0)
		if b0[9] == 1 {
			fmt.Println("reading ICMP packet")
			fmt.Println(hex.Dump(b0[:n]))
		}
	}
}
