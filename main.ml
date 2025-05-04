open Utils

let main () =
  if Array.length Sys.argv < 4 then 
    Printf.eprintf "Usage: %s <tunX> <tun CIDR>" Sys.argv.(0);
  let tun_name = Sys.argv.(1) in 
  let tun_addr = Sys.argv.(2) in 
  let tun = Tun.create tun_name in
  run_command (Printf.sprintf "sudo ip addr add %s dev %s" tun_addr tun_name);
  run_command (Printf.sprintf "ip link set %s up" tun_name);
  
  let buf = Bytes.create 1500 in
  while true do
    flush stdout;
    let packet_size = Tun.read tun buf in
    let packet_bytes = Bytes.sub buf 0 packet_size in
    let packet = Ip.deserialize packet_bytes in
    match (packet.version, packet.protocol) with 
    
    | (Ip.IPv4, Ip.TCP) ->  
      Ip.pp_ip_packet (Format.formatter_of_out_channel stdout) packet;
      flush stdout;
    | _ -> ()
  done

let () = main () 