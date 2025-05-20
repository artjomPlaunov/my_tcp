type t = {
  version : version;
  ihl : int;
  total_length : int;
  source_addr : int32;
  dest_addr : int32;
  payload : Bytes.t;
  protocol : protocol;
}

and version = IPv4 | IPv6
and protocol = ICMP | IGMP | TCP | UDP | Other of int

let version_offset = 0

let ipv4_to_string addr =
  Printf.sprintf "%d.%d.%d.%d"
    (Int32.to_int (Int32.shift_right_logical addr 24))
    (Int32.to_int
       (Int32.shift_right_logical (Int32.logand addr 0x00FF0000l) 16))
    (Int32.to_int (Int32.shift_right_logical (Int32.logand addr 0x0000FF00l) 8))
    (Int32.to_int (Int32.logand addr 0x000000FFl))

let string_to_ipv4 s =
  if s = "" then 0l
  else
    let parts = String.split_on_char '.' s in
    match List.map int_of_string parts with
    | [ b0; b1; b2; b3 ] ->
        let addr =
          Int32.logor
            (Int32.shift_left (Int32.of_int b0) 24)
            (Int32.logor
               (Int32.shift_left (Int32.of_int b1) 16)
               (Int32.logor
                  (Int32.shift_left (Int32.of_int b2) 8)
                  (Int32.of_int b3)))
        in
        addr
    | _ -> failwith "Invalid IPv4 address format"

let deserialize_version buf =
  match (Bytes.get buf 0 |> int_of_char) lsr 4 with
  | 4 -> IPv4
  | 6 -> IPv6
  | _ -> failwith "Invalid IP version"

let serialize_version v =
  match v with IPv4 -> Int32.of_int 4 | IPv6 -> Int32.of_int 6

let serialize_protocol p =
  match p with TCP -> 6 | ICMP -> 1 | _ -> failwith "serialize_protocol todo"

let deserialize buf =
  let version = deserialize_version buf in
  let ihl = (Bytes.get buf 0 |> int_of_char) land 0x0f in
  let total_length = Bytes.length buf in
  let source_addr = Bytes.get_int32_be buf 12 in
  let dest_addr = Bytes.get_int32_be buf 16 in
  let payload = Bytes.sub buf (ihl * 4) (total_length - (ihl * 4)) in
  let protocol =
    match Bytes.get buf 9 |> int_of_char with
    | 1 -> ICMP
    | 2 -> IGMP
    | 6 -> TCP
    | 17 -> UDP
    | _ -> Other (Bytes.get buf 9 |> int_of_char)
  in
  { version; ihl; total_length; source_addr; dest_addr; payload; protocol }

let serialize ~protocol ~source ~dest ~payload =
  let protocol = serialize_protocol protocol in
  let source = string_to_ipv4 source in
  let dest = string_to_ipv4 dest in
  let version_ihl = 0x45 in
  let tos = 0 in
  let total_len = 20 + Bytes.length payload in
  let id = 78 in
  let flags_fragment = 0 in
  let ttl = 64 in
  let header_checksum = 0 in

  let header = Bytes.create 20 in
  Bytes.set header 0 (Char.chr version_ihl);
  Bytes.set header 1 (Char.chr tos);
  Bytes.set_int16_be header 2 total_len;
  Bytes.set_int16_be header 4 id;
  Bytes.set_int16_be header 6 flags_fragment;
  Bytes.set header 8 (Char.chr ttl);
  Bytes.set header 9 (Char.chr protocol);
  Bytes.set_int16_be header 10 header_checksum;
  Bytes.set_int32_be header 12 source;
  Bytes.set_int32_be header 16 dest;

  let checksum =
    let sum = ref 0 in
    let i = ref 0 in
    while !i < 20 do
      let word =
        (Char.code (Bytes.get header !i) lsl 8)
        + Char.code (Bytes.get header (!i + 1))
      in
      sum := !sum + word;
      i := !i + 2
    done;
    let sum = (!sum lsr 16) + (!sum land 0xFFFF) in
    let sum = sum + (sum lsr 16) in
    lnot sum land 0xFFFF
  in
  Bytes.set_int16_be header 10 checksum;
  let packet = Bytes.create (20 + Bytes.length payload) in
  Bytes.blit header 0 packet 0 20;
  Bytes.blit payload 0 packet 20 (Bytes.length payload);
  packet

let pp_ip_packet fmt packet =
  let version_str =
    match packet.version with IPv4 -> "IPv4" | IPv6 -> "IPv6"
  in
  Format.fprintf fmt "IP Packet:@\n";
  Format.fprintf fmt "  Version: %s@\n" version_str;
  Format.fprintf fmt "  IHL: %d (words)@\n" packet.ihl;
  Format.fprintf fmt "  Total Length: %d bytes@\n" packet.total_length;
  Format.fprintf fmt "  Source: %s@\n" (ipv4_to_string packet.source_addr);
  Format.fprintf fmt "  Destination: %s@\n" (ipv4_to_string packet.dest_addr);
  Format.fprintf fmt "  Payload Length: %d bytes@\n"
    (Bytes.length packet.payload);
  Format.fprintf fmt "  Protocol: %s@\n"
    (match packet.protocol with
    | ICMP -> "ICMP"
    | TCP -> "TCP"
    | UDP -> "UDP"
    | IGMP -> "IGMP"
    | Other p -> Printf.sprintf "Other (%d)" p)

let show_ip_packet packet = Format.asprintf "%a" pp_ip_packet packet
