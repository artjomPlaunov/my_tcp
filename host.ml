let main env = 
  if Array.length Sys.argv < 4 then 
    Printf.eprintf "Usage: %s <tunX> <tun CIDR>" Sys.argv.(0);
  let tun_name = Sys.argv.(1) in 
  let tun_addr = Sys.argv.(2) in
  let msg_stream = Tcp.tcp_init env tun_name tun_addr in 
  Eio.traceln "Client";
  while true do 
    Eio.traceln "Write Conn Msg.";
    let c = (Tcp.Conn {ip="10.0.1.7"; port=123}) in 
    Eio.Stream.add msg_stream c;
  done
  
 
let () = Eio_main.run main



