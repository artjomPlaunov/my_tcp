type t = {
  source : int;
  dest : int;
  seq_num : int32;
  ack_num : int32;
  window : int;
  flags : flags;
  payload : Bytes.t;
}

and flags = {
  urg : bool;
  ack : bool;
  psh : bool;
  rst : bool;
  syn : bool;
  fin : bool;
}

let get_flags x =
  let urg = if x land 32 <> 0 then true else false in
  let ack = if x land 16 <> 0 then true else false in
  let psh = if x land 8 <> 0 then true else false in
  let rst = if x land 4 <> 0 then true else false in
  let syn = if x land 2 <> 0 then true else false in
  let fin = if x land 1 <> 0 then true else false in
  { urg; ack; psh; rst; syn; fin }

let serialize_flags flags =
  0 |> fun r ->
  if flags.urg then r lor 32
  else
    r |> fun r ->
    if flags.ack then r lor 16
    else
      r |> fun r ->
      if flags.psh then r lor 8
      else
        r |> fun r ->
        if flags.rst then r lor 4
        else
          r |> fun r ->
          if flags.syn then r lor 2
          else r |> fun r -> if flags.fin then r lor 1 else r

let serialize t =
  let data_len = Bytes.length t.payload in
  let hdr_flags = 5 in
  let hdr_flags = hdr_flags lsl 12 in
  let hdr_flags = hdr_flags lor serialize_flags t.flags in
  let buf = Bytes.make (20 + data_len) '\x00' in
  Bytes.set_int16_be buf 0 t.source;
  Bytes.set_int16_be buf 2 t.dest;
  Bytes.set_int32_be buf 4 t.seq_num;
  Bytes.set_int32_be buf 8 t.ack_num;
  Bytes.set_int16_be buf 12 hdr_flags;
  Bytes.blit t.payload 0 buf 20 data_len;
  buf

let checksum buf src dest =
  let pseudo = Bytes.make 12 '\x00' in
  Bytes.set_int32_be pseudo 0 src;
  Bytes.set_int32_be pseudo 4 dest;
  Bytes.set_int16_be pseudo 8 6;
  Bytes.set_int16_be pseudo 10 (Bytes.length buf);
  let total_len = Bytes.length pseudo + Bytes.length buf in
  let rec sum i acc =
    if i >= total_len then acc
    else
      let b1 =
        if i < Bytes.length pseudo then int_of_char (Bytes.get pseudo i)
        else int_of_char (Bytes.get buf (i - Bytes.length pseudo))
      in
      let b2 =
        if i + 1 < total_len then
          if i + 1 < Bytes.length pseudo then
            int_of_char (Bytes.get pseudo (i + 1))
          else int_of_char (Bytes.get buf (i + 1 - Bytes.length pseudo))
        else 0
      in
      let word = (b1 lsl 8) lor b2 in
      let acc = acc + word in
      let acc = (acc land 0xFFFF) + (acc lsr 16) in
      sum (i + 2) acc
  in
  let raw_sum = sum 0 0 in
  lnot raw_sum land 0xFFFF

let deserialize (buf : Bytes.t) : t =
  if Bytes.length buf < 20 then
    invalid_arg "Buffer too short to be a TCP segment";

  let source = Bytes.get_uint16_be buf 0 in
  let dest = Bytes.get_uint16_be buf 2 in
  let seq_num = Bytes.get_int32_be buf 4 in
  let ack_num = Bytes.get_int32_be buf 8 in
  let hdr_flags = Bytes.get_uint16_be buf 12 in

  let flags_bits = hdr_flags land 0x3F in
  (* lower 6 bits: flags *)
  let flags = get_flags flags_bits in

  let payload_len = Bytes.length buf - 20 in
  let payload = Bytes.sub buf 20 payload_len in

  { source; dest; seq_num; ack_num; window = 0; flags; payload }

let pp_flags f =
  Printf.sprintf "URG:%b ACK:%b PSH:%b RST:%b SYN:%b FIN:%b" f.urg f.ack f.psh
    f.rst f.syn f.fin

let pp_tcp (pkt : t) =
  Printf.printf "TCP Packet:\n";
  Printf.printf "  Source Port    : %d\n" pkt.source;
  Printf.printf "  Dest Port      : %d\n" pkt.dest;
  Printf.printf "  Seq Number     : %ld\n" pkt.seq_num;
  Printf.printf "  Ack Number     : %ld\n" pkt.ack_num;
  Printf.printf "  Window         : %d\n" pkt.window;
  Printf.printf "  Flags          : %s\n" (pp_flags pkt.flags);
  Printf.printf "  Payload Length : %d bytes\n" (Bytes.length pkt.payload);
  if Bytes.length pkt.payload > 0 then
    let printable =
      Bytes.to_seq pkt.payload
      |> Seq.map (fun c ->
             let i = Char.code c in
             if i >= 32 && i <= 126 then c else '.')
      |> String.of_seq
    in
    Printf.printf "  Payload (ASCII): \"%s\"\n" printable
