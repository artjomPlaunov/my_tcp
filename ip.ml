type version = IPv4 | IPv6

type protocol = 
  | ICMP
  | IGMP
  | TCP
  | UDP
  | Other of int

type t = {
  version : version;
  ihl : int;
  total_length : int;
  source_addr : int32;
  dest_addr : int32;
  payload : Bytes.t;
  protocol : protocol;
}

let ipv4_to_string addr =
  let b0 = Int32.to_int (Int32.shift_right_logical addr 24) in
  let b1 = Int32.to_int (Int32.shift_right_logical (Int32.logand addr 0x00FF0000l) 16) in
  let b2 = Int32.to_int (Int32.shift_right_logical (Int32.logand addr 0x0000FF00l) 8) in
  let b3 = Int32.to_int (Int32.logand addr 0x000000FFl) in
  Printf.sprintf "%d.%d.%d.%d" b0 b1 b2 b3

let string_to_ipv4 s =
  let parts = String.split_on_char '.' s in
  match List.map int_of_string parts with
  | [b0; b1; b2; b3] ->
      let addr = Int32.logor
        (Int32.shift_left (Int32.of_int b0) 24)
        (Int32.logor
          (Int32.shift_left (Int32.of_int b1) 16)
          (Int32.logor
            (Int32.shift_left (Int32.of_int b2) 8)
            (Int32.of_int b3)))
      in
      addr
  | _ -> failwith "Invalid IPv4 address format"

let parse_version buf = 
  match (Bytes.get buf 0 |> int_of_char) lsr 4 with
  | 4 -> IPv4
  | 6 -> IPv6
  | _ -> failwith "Invalid IP version"

let parse_ip_address buf start =
  Int32.logor
    (Int32.shift_left (Int32.of_int (Bytes.get buf start |> int_of_char)) 24)
    (Int32.logor
      (Int32.shift_left (Int32.of_int (Bytes.get buf (start + 1) |> int_of_char)) 16)
      (Int32.logor
        (Int32.shift_left (Int32.of_int (Bytes.get buf (start + 2) |> int_of_char)) 8)
        (Int32.of_int (Bytes.get buf (start + 3) |> int_of_char))))

let parse_ip_packet buf = 
  let version = parse_version buf in
  let ihl = (Bytes.get buf 0 |> int_of_char) land 0x0f in
  let total_length = Bytes.length buf in 
  let source_addr = parse_ip_address buf 12 in
  let dest_addr = parse_ip_address buf 16 in
  let payload = Bytes.sub buf (ihl * 4) (total_length - ihl * 4) in
  let protocol = match Bytes.get buf 9 |> int_of_char with
    | 1 -> ICMP
    | 2 -> IGMP
    | 6 -> TCP
    | 17 -> UDP
    | _ -> Other (Bytes.get buf 9 |> int_of_char)
  in
  { 
    version; 
    ihl; 
    total_length; 
    source_addr;
    dest_addr;
    payload;
    protocol;
  }

let pp_ip_packet fmt packet =
  let version_str = match packet.version with IPv4 -> "IPv4" | IPv6 -> "IPv6" in
  Format.fprintf fmt "IP Packet:@\n";
  Format.fprintf fmt "  Version: %s@\n" version_str;
  Format.fprintf fmt "  IHL: %d (words)@\n" packet.ihl;
  Format.fprintf fmt "  Total Length: %d bytes@\n" packet.total_length;
  Format.fprintf fmt "  Source: %s@\n" (ipv4_to_string packet.source_addr);
  Format.fprintf fmt "  Destination: %s@\n" (ipv4_to_string packet.dest_addr);
  Format.fprintf fmt "  Payload Length: %d bytes@\n" (Bytes.length packet.payload);
  Format.fprintf fmt "  Protocol: %s@\n" (match packet.protocol with
    | ICMP -> "ICMP"
    | TCP -> "TCP"
    | UDP -> "UDP"
    | IGMP -> "IGMP"
    | Other p -> Printf.sprintf "Other (%d)" p)

let show_ip_packet packet =
  Format.asprintf "%a" pp_ip_packet packet
