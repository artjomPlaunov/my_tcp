let main () =
  if Array.length Sys.argv < 4 then 
    Printf.eprintf "Usage: %s <tunX> <tun CIDR>" Sys.argv.(0);
  let tun_name = Sys.argv.(1) in 
  let tun_addr = Sys.argv.(2) in 
  let _ = Tcp.tcp_open 
            ~active:true 
            "10.0.1.7" 6789 
            tun_name tun_addr  
            ~dest_ip:"10.0.0.5" ~dest_port:1234 () 
  in 
  while true do
    ()
  done

let () = main ()
