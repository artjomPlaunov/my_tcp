let hexdump buf size =
  let ihl = (Char.code (Bytes.get buf 0) land 0x0f) * 4 in  (* IHL is in 32-bit words, multiply by 4 for bytes *)
  Printf.printf "\nRaw IP packet bytes:\n";
  Printf.printf "Header (IHL: %d bytes):\n" ihl;
  for i = 0 to min (ihl - 1) (size - 1) do  (* Show header based on IHL *)
    Printf.printf "%02x " (Char.code (Bytes.get buf i));
    if (i + 1) mod 4 = 0 then Printf.printf "\n"
  done;
  if size > ihl then (
    Printf.printf "\nPayload:\n";
    for i = ihl to size - 1 do
      Printf.printf "%02x " (Char.code (Bytes.get buf i));
      if (i + 1) mod 4 = 0 then Printf.printf "\n"
    done
  );
  Printf.printf "\n%!"

let should_process_packet buf =
  let version = (Char.code (Bytes.get buf 0) lsr 4) in  (* Version is in high nibble of first byte *)
  let protocol = Char.code (Bytes.get buf 9) in  (* Protocol is at offset 9 in IP header *)
  version = 4 && protocol <> 0x11  (* IPv4 and not UDP *)

let run_command cmd =
  Printf.printf "Running command: %s\n" cmd;
  match Unix.system cmd with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith ("Command failed: " ^ cmd)

let main () =
  let tun = Tun.create "tun0" in
  
  (* Configure IP and bring interface up *)
  run_command "ip addr add 10.1.2.1/24 dev tun0";
  run_command "ip link set tun0 up";
  Printf.printf "Configured tun0 with IP 10.1.2.1/24 and brought interface to operational state\n%!";
  
  let buf = Bytes.create 2000 in
  while true do 
    flush stdout;
    let packet_size = Tun.read tun buf in
    let packet_bytes = Bytes.sub buf 0 packet_size in
    if should_process_packet packet_bytes then (
      let ip_packet = Ip.parse_ip_packet packet_bytes in
      Printf.printf "\n=== Parsed IP Packet ===\n";
      Ip.pp_ip_packet (Format.formatter_of_out_channel stdout) ip_packet;
      Printf.printf "\n=== Raw Packet Hexdump ===\n";
      hexdump buf packet_size;
      flush stdout
    )
  done

let () = main () 