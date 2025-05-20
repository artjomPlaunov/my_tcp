open Eio
open Utils
module Packet = Packet

type state = LISTEN | SYN_SENT | SYN_RCVD
type conn = { ip : string; port : int }
type msg = Conn of conn

(*  Transmission Control Block 
    Stores TCP connection information
*)
type tcb = {
  tun : Tun.t;
  mutable state : state;
  port : int;
  ip : int32;
  mutable dest_ip : int32;
  mutable dest_port : int;
  mutable iss : int32;
}

(* 
  Dummy initial sequence number generator. 
  Not compliant with specs. 
*)
let gen_isn () = 100l

(*  Validation checks packet destination (ip, port), 
    and performs TCP checksum.*)
let validate_packet tcb (packet : Ip.t) (pkt : Packet.t) pkt_bytes =
  traceln "validating packet";
  let original_checksum = Bytes.get_int16_be pkt_bytes 16 in
  Bytes.set_int16_be pkt_bytes 16 0;
  let checksum =
    Packet.checksum pkt_bytes packet.source_addr packet.dest_addr
  in
  tcb.ip = packet.dest_addr && pkt.dest = tcb.port
  && original_checksum = checksum

let packet_handler tcb (packet : Ip.t) tcp_pkt =
  match tcb.state with
  | LISTEN ->
      traceln "(LISTEN) read_packet";
      Packet.pp_tcp tcp_pkt;
      flush stdout;
      if tcp_pkt.flags.syn then tcb.dest_ip <- packet.source_addr;
      tcb.dest_port <- tcp_pkt.source;
      let syn_ack_pkt : Packet.t =
        {
          source = tcb.port;
          dest = tcb.dest_port;
          seq_num = 300l;
          ack_num = Int32.add tcp_pkt.seq_num 1l;
          window = 10;
          flags = Packet.get_flags 18;
          payload = Bytes.create 0;
        }
      in
      let payload = Packet.serialize syn_ack_pkt in
      let checksum = Packet.checksum payload tcb.ip tcb.dest_ip in
      Bytes.set_int16_be payload 16 checksum;
      let pkt =
        Ip.serialize ~protocol:Ip.TCP ~source:(Ip.ipv4_to_string tcb.ip)
          ~dest:(Ip.ipv4_to_string tcb.dest_ip)
          ~payload
      in
      let _ = Tun.write tcb.tun pkt in
      tcb.state <- SYN_RCVD;
      traceln "(LISTEN) -> (SYN-RCVD) transition";
      ()
  | SYN_SENT ->
      traceln "(SYN_SENT) read_packet";
      Packet.pp_tcp tcp_pkt;
      flush stdout;
      (* Packet validation only checks destination, so check if packet is 
         from valid source, with expected ack and control bits set. *)
      if
        tcb.dest_port = tcp_pkt.source
        && tcb.dest_ip = tcb.dest_ip && tcp_pkt.flags.syn && tcp_pkt.flags.ack
        && tcp_pkt.ack_num = Int32.add 1l tcb.iss
      then 
        traceln "(SYN_SENT) -> (ESTABLISHED) transition";
        ()
  | SYN_RCVD -> ()

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
      seq_num = tcb.iss;
      ack_num = 0l;
      window = 10;
      flags = Packet.get_flags 2;
      payload = Bytes.create 0;
    }
  in
  let payload = Packet.serialize syn_pkt in
  let checksum = Packet.checksum payload tcb.ip tcb.dest_ip in
  Bytes.set_int16_be payload 16 checksum;
  let pkt =
    Ip.serialize ~protocol:Ip.TCP ~source:(Ip.ipv4_to_string tcb.ip)
      ~dest:(Ip.ipv4_to_string tcb.dest_ip)
      ~payload
  in
  let _ = Tun.write tcb.tun pkt in
  ()

let tcp_server _ _ ~active addr port tun_name tun_addr msg_stream
    ?(dest_ip = "") ?(dest_port = -1) () =
  let tun = Tun.create tun_name in
  run_command (Printf.sprintf "sudo ip addr add %s dev %s" tun_addr tun_name);
  run_command (Printf.sprintf "ip link set %s up" tun_name);
  let tcb =
    {
      tun;
      state = LISTEN;
      port;
      ip = addr;
      dest_ip = Ip.string_to_ipv4 dest_ip;
      dest_port;
      iss = gen_isn ();
    }
  in
  traceln "TCP Instantiated, TCB Created";
  if active then (
    init_handshake tcb;
    traceln "(CLOSED) -> (SYN_SENT) transition";
    tcb.state <- SYN_SENT)
  else traceln "(CLOSED) -> (LISTEN) transition";

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
