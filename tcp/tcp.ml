open Eio
open Utils
module Packet = Packet

type state = LISTEN | SYN_SENT
type conn = { ip : string; port : int }
type msg = Conn of conn

(*  Transmission Control Block 
    Stores TCP connection information
*)
type tcb = {
  tun : Tun.t;
  mutable state : state;
  port : int;
  ip : string;
  mutable dest_ip : string;
  mutable dest_port : int;
}

(* 
  Dummy initial sequence number generator. 
  Not compliant with specs. 
*)
let gen_isn () = 69l

let validate_packet tcb (packet : Ip.t) (pkt : Packet.t) pkt_bytes =
  traceln "validating packet";
  let ip = Ip.string_to_ipv4 tcb.ip in
  let original_checksum = Bytes.get_int16_be pkt_bytes 16 in
  Bytes.set_int16_be pkt_bytes 16 0;
  let checksum =
    Packet.checksum pkt_bytes packet.source_addr packet.dest_addr
  in
  ip = packet.dest_addr && pkt.dest = tcb.port && original_checksum = checksum

let packet_handler tcb (_ : Ip.t) tcp_pkt =
  match tcb.state with
  | LISTEN ->
      traceln "(LISTEN) read_packet";
      Packet.pp_tcp tcp_pkt;
      flush stdout
  | SYN_SENT -> ()

let read_packet tcb () =
  traceln "EIO read_packet spawned";
  let buf = Bytes.create 1500 in
  while true do
    let packet_size = Tun.read tcb.tun buf in
    let packet_bytes = Bytes.sub buf 0 packet_size in
    let packet = Ip.deserialize packet_bytes in
    match (packet.version, packet.protocol) with
    | Ip.IPv4, Ip.TCP ->
        let tcp_pkt = Packet.deserialize packet.payload in
        if validate_packet tcb packet tcp_pkt packet.payload then
          packet_handler tcb packet tcp_pkt
    | _ ->
        ();
        Fiber.yield ()
  done;
  ()

let read_cmd _ msg_stream () =
  traceln "EIO read_cmd spawned";
  while true do
    let msg = Eio.Stream.take_nonblocking msg_stream in
    match msg with
    | Some (Conn _) -> traceln "Got Conn!"
    | None -> Fiber.yield ()
  done;
  ()

let init_handshake tcb =
  let syn_pkt : Packet.t =
    {
      source = tcb.port;
      dest = tcb.dest_port;
      seq_num = gen_isn ();
      ack_num = 0l;
      window = 10;
      flags = Packet.get_flags 2;
      payload = Bytes.create 0;
    }
  in
  let payload = Packet.serialize syn_pkt in
  let checksum =
    Packet.checksum payload (Ip.string_to_ipv4 tcb.ip)
      (Ip.string_to_ipv4 tcb.dest_ip)
  in
  Bytes.set_int16_be payload 16 checksum;
  let pkt =
    Ip.serialize ~protocol:Ip.TCP ~source:tcb.ip ~dest:tcb.dest_ip ~payload
  in
  let _ = Tun.write tcb.tun pkt in
  ()

let tcp_server _ _ ~active addr port tun_name tun_addr msg_stream
    ?(dest_ip = "") ?(dest_port = -1) () =
  let tun = Tun.create tun_name in
  run_command (Printf.sprintf "sudo ip addr add %s dev %s" tun_addr tun_name);
  run_command (Printf.sprintf "ip link set %s up" tun_name);
  let tcb = { tun; state = LISTEN; port; ip = addr; dest_ip; dest_port } in
  if active then (
    traceln "TCP Server (Active)";
    init_handshake tcb;
    tcb.state <- SYN_SENT)
  else traceln "TCP Server (Passive)";

  Eio.Fiber.both (read_packet tcb) (read_cmd tcb msg_stream)

let tcp_open ~active addr port tun_name tun_addr ?(dest_ip = "")
    ?(dest_port = -1) () =
  let msg_stream = Eio.Stream.create 3 in
  ignore
    (Domain.spawn (fun () ->
         Eio_main.run (fun env ->
             Eio.Switch.run (fun sw ->
                 tcp_server env sw ~active addr port tun_name tun_addr
                   msg_stream ~dest_ip ~dest_port ()))));
  msg_stream
