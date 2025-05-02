open Utils

(* let is_ipv4_ping packet =
  Bytes.length packet >= 20 &&  (* At least IPv4 header *)
  Bytes.get packet 0 = '\x45' &&  (* Version 4, IHL 5 *)
  Bytes.get packet 9 = '\x01'     (* Protocol = ICMP (1) *)

let is_custom_protocol packet =
  Bytes.length packet >= 20 &&  (* At least IPv4 header *)
  Bytes.get packet 0 = '\x45' &&  (* Version 4, IHL 5 *)
  Bytes.get packet 9 = '\x99'     Protocol = 153 *)

let main () =
  let tun = Tun.create "tun0" in
  
  (* Configure IP and bring interface up *)
  run_command "sudo ip addr add 10.0.0.1/24 dev tun0";
  run_command "ip link set tun0 up";
  
  
  let buf = Bytes.create 1500 in
  while true do
    flush stdout;
    let packet_size = Tun.read tun buf in
    let packet_bytes = Bytes.sub buf 0 packet_size in
      let ip_packet = Ip.parse_ip_packet packet_bytes in
      Printf.printf "\n=== Received ICMP Packet ===\n";
      Ip.pp_ip_packet (Format.formatter_of_out_channel stdout) ip_packet;
      Printf.printf "\n=== Raw Packet Hexdump ===\n";
      (* hexdump buf packet_size; *)
      flush stdout
    
  done

let () = main () 