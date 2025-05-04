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

let run_command cmd =
  Printf.printf "Running command: %s\n" cmd;
  match Unix.system cmd with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith ("Command failed: " ^ cmd)

(* read scapy generated TCP backet from raw binary. *)
let read_tcp_payload filename =
  (* Open the file *)
  let in_channel = open_in_bin filename in
  let length = in_channel_length in_channel in
  (* Read the entire file into a bytes array *)
  let payload = Bytes.create length in
  really_input in_channel payload 0 length;
  close_in in_channel;
  payload