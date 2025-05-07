type t = {
  source: int;
  dest: int;
  seq_num: int32;
  ack_num: int32;
  header_len: int;
  window: int;
  checksum: int;
  payload: Bytes.t;
}
and 
flags = {
  urg: bool;
  ack: bool;
  psh: bool;
  rst: bool;
  syn: bool;
  fin: bool;
}

let serialize_flags flags = 
  0 
  |> fun r -> if flags.urg then r lor 32 else r 
  |> fun r -> if flags.ack then r lor 16 else r 
  |> fun r -> if flags.psh then r lor 8 else r 
  |> fun r -> if flags.rst then r lor 4 else r 
  |> fun r -> if flags.syn then r lor 2 else r
  |> fun r -> if flags.fin then r lor 1 else r

let serialize t = 
  let data_len = Bytes.length t.payload in 
  let hdr_flags = t.header_len in
  let hdr_flags = hdr_flags lsl 12 in 
  let buf = Bytes.create (t.header_len + data_len) in 
  Bytes.set_int16_be buf 0 t.source;
  Bytes.set_int16_be buf 2 t.dest;
  Bytes.set_int32_be buf 4 t.seq_num;
  Bytes.set_int32_be buf 8 t.ack_num;
  Bytes.set_int16_be buf 12 hdr_flags;

  buf
