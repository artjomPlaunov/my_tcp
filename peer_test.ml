open Utils 

let read_tcp_payload filename =
  (* Open the file *)
  let in_channel = open_in_bin filename in
  let length = in_channel_length in_channel in
  (* Read the entire file into a bytes array *)
  let payload = Bytes.create length in
  really_input in_channel payload 0 length;
  close_in in_channel;
  payload

let main () =
  if Array.length Sys.argv < 4 then 
    Printf.eprintf "Usage: %s <tunX> <tun CIDR>" Sys.argv.(0);
  let tun_name = Sys.argv.(1) in 
  let tun_addr = Sys.argv.(2) in 
  let tun = Tun.create tun_name in
  run_command (Printf.sprintf "sudo ip addr add %s dev %s" tun_addr tun_name);
  run_command (Printf.sprintf "ip link set %s up" tun_name);
  (* dummy TCP packet *)
  let payload = read_tcp_payload "tcp.bin" in
  let packet = Ip.serialize ~protocol:Ip.TCP ~source:"10.0.1.7" ~dest:"10.0.0.6" ~payload in 
  let p = Ip.deserialize packet in 
  
  flush stdout;
  while true do
    let _ = Tun.write tun packet in
    flush stdout;
    let _ = Ip.pp_ip_packet (Format.formatter_of_out_channel stdout) p in 
    Unix.sleep 1 
  done

let () = main ()
