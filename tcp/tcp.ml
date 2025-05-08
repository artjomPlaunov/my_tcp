open Eio
open Utils

module Packet = Packet

type t = {
  tun: Tun.t;
}

type conn = { ip : string; port : int }

type msg = Conn of conn

let tcp_server _ _ tun_name tun_addr msg_stream = 
  traceln "TCP Server";
  let tun = Tun.create tun_name in
  run_command (Printf.sprintf "sudo ip addr add %s dev %s" tun_addr tun_name);
  run_command (Printf.sprintf "ip link set %s up" tun_name);

  (* Handle TUN device input. *)
  let tun_fiber = (fun () -> 
    traceln "Read TUN Packet";
    let buf = Bytes.create 1500 in
    while true do
      let packet_size = Tun.read tun buf in
      let packet_bytes = Bytes.sub buf 0 packet_size in
      let packet = Ip.deserialize packet_bytes in
      match (packet.version, packet.protocol) with 
      | (Ip.IPv4, Ip.TCP) ->  
        Ip.pp_ip_packet (Format.formatter_of_out_channel stdout) packet;
        flush stdout;
      | _ -> ();
      Fiber.yield ();
    done;
    ()
  ) in

  (* Handle user communication. *)
  let user_fiber = (fun () -> 
    traceln "Read Client Msg.";
    while true do 
      let msg = Eio.Stream.take_nonblocking msg_stream in 
      match msg with 
      | Some (Conn _) -> traceln "Got Conn!";
      | None -> Fiber.yield ();
    done;
  ) in

  (* Wait for both fibers to finish (although they have infinite loops, we await them to ensure the program doesn't exit). *)
  Eio.Fiber.both tun_fiber user_fiber


let tcp_init _ tun_name tun_addr =
  let msg_stream = Eio.Stream.create 3 in  
  ignore (Domain.spawn (fun () -> 
    Eio_main.run ( fun env -> 
      Eio.Switch.run (fun sw -> 
        tcp_server env sw tun_name tun_addr msg_stream;
      );
    )
  ));
  msg_stream



  
  