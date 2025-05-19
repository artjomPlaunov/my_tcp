open Eio
open Utils

module Packet = Packet

type state = LISTEN | SYN_SENT

type conn = { ip : string; port : int }

type msg = Conn of conn 

type tcb = {
  mutable state : state; 
  port: int;
  ip : string;
  mutable dest_ip : string;
  mutable dest_port : int;
}

let tcp_server _ _ 
  ~active 
  addr port
  tun_name tun_addr 
  msg_stream   
  ?(dest_ip="") ?(dest_port=(-1)) () = 
  let tcb = 
    {
      state = LISTEN; 
      port = port;
      ip = addr; 
      dest_ip = dest_ip; 
      dest_port = dest_port;
    } 
  in 
  if active 
  then (
    traceln "Instantiating TCP Server with Active Open";
    tcb.state <- SYN_SENT;
  );
  let tun = Tun.create tun_name in
  run_command (Printf.sprintf "sudo ip addr add %s dev %s" tun_addr tun_name);
  run_command (Printf.sprintf "ip link set %s up" tun_name);

  let read_packet_thread = (fun () -> 
    traceln "EIO Fiber: Read Packet from TUN Device";
    let buf = Bytes.create 1500 in
    while true do
      let packet_size = Tun.read tun buf in
      let packet_bytes = Bytes.sub buf 0 packet_size in
      let packet = Ip.deserialize packet_bytes in
      match (packet.version, packet.protocol) with 
      | (Ip.IPv4, Ip.TCP) ->  
        (match tcb.state with 
        | LISTEN -> 
          traceln "read_packet_thread: reading packets from TUN device in LISTEN state";
          Ip.pp_ip_packet (Format.formatter_of_out_channel stdout) packet;
          flush stdout;
        | _ -> ()
        )
      | _ -> ();
      Fiber.yield ();
    done;
    ()
  ) in

  let read_user_cmd_thread = (fun () -> 
    traceln "Read Client Msg.";
    while true do 
      let msg = Eio.Stream.take_nonblocking msg_stream in 
      match msg with 
      | Some (Conn _) -> traceln "Got Conn!";
      | None -> Fiber.yield ();
    done;
  ) in
  
  if tcb.state = SYN_SENT 
  then 
    (* source, *)
    ();
  Eio.Fiber.both read_packet_thread read_user_cmd_thread

let tcp_open 
      ~active 
      addr port 
      tun_name tun_addr  
      ?(dest_ip="") ?(dest_port=(-1)) () =
  (* Channel of communication between client and TCP server, 
     Both Client and TCP server can read/write from msg_stream. 
  *)
  let msg_stream = Eio.Stream.create 3 in  
  ignore (Domain.spawn (fun () -> 
    Eio_main.run ( fun env -> 
      Eio.Switch.run (fun sw -> 
        tcp_server 
          env sw
          ~active
          addr port
          tun_name tun_addr 
          msg_stream
          ~dest_ip ~dest_port (); 
      );
    )
  ));
  msg_stream



  
  