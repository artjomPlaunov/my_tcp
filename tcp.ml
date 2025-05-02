type tcp_state =
  | CLOSED
  | LISTEN
  | SYN_SENT
  | SYN_RECEIVED
  | ESTABLISHED

type tcp_header = {
  source_port : int;
  dest_port : int;
  seq_num : int32;
  ack_num : int32;
  data_offset : int;
  flags : int;
  window : int;
  checksum : int;
  urgent_ptr : int;
}

let create_tcp_header ~source_port ~dest_port ~seq_num ~ack_num ~flags =
  {
    source_port;
    dest_port;
    seq_num;
    ack_num;
    data_offset = 5;  (* 20 bytes / 4 = 5 words *)
    flags;
    window = 65535;   (* Maximum window size *)
    checksum = 0;     (* Will be calculated later *)
    urgent_ptr = 0;
  }

let serialize_tcp_header header =
  let buf = Bytes.create 20 in
  (* Source port *)
  Bytes.set_int16_be buf 0 header.source_port;
  (* Destination port *)
  Bytes.set_int16_be buf 2 header.dest_port;
  (* Sequence number *)
  Bytes.set_int32_be buf 4 header.seq_num;
  (* Acknowledgment number *)
  Bytes.set_int32_be buf 8 header.ack_num;
  (* Data offset and flags *)
  let flags_byte = (header.data_offset lsl 4) lor header.flags in
  Bytes.set buf 12 (Char.chr flags_byte);
  (* Window size *)
  Bytes.set_int16_be buf 14 header.window;
  (* Checksum (will be calculated later) *)
  Bytes.set_int16_be buf 16 header.checksum;
  (* Urgent pointer *)
  Bytes.set_int16_be buf 18 header.urgent_ptr;
  buf

let calculate_tcp_checksum ~source_ip ~dest_ip ~tcp_segment =
  (* TODO: Implement TCP checksum calculation *)
  0

let create_syn_packet ~source_port ~dest_port ~seq_num =
  let header = create_tcp_header
    ~source_port
    ~dest_port
    ~seq_num
    ~ack_num:0l
    ~flags:0x02  (* SYN flag *)
  in
  serialize_tcp_header header

let create_syn_ack_packet ~source_port ~dest_port ~seq_num ~ack_num =
  let header = create_tcp_header
    ~source_port
    ~dest_port
    ~seq_num
    ~ack_num
    ~flags:0x12  (* SYN and ACK flags *)
  in
  serialize_tcp_header header

let create_ack_packet ~source_port ~dest_port ~seq_num ~ack_num =
  let header = create_tcp_header
    ~source_port
    ~dest_port
    ~seq_num
    ~ack_num
    ~flags:0x10  (* ACK flag *)
  in
  serialize_tcp_header header 