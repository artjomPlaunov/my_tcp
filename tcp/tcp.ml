open Utils

module Packet = Packet

type t = {
  tun: Tun.t;
}

let tcp_init tun_name tun_addr = 
  let tun = Tun.create tun_name in
  run_command (Printf.sprintf "sudo ip addr add %s dev %s" tun_addr tun_name);
  run_command (Printf.sprintf "ip link set %s up" tun_name);
  { tun }
  