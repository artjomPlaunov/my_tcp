let hexdump buf =
  let len = Bytes.length buf in
  for i = 0 to len - 1 do
    if i mod 16 = 0 then Printf.printf "\n%04x: " i;
    Printf.printf "%02x " (int_of_char (Bytes.get buf i))
  done;
  Printf.printf "\n"

let run_command cmd =
  Printf.printf "Running command: %s\n" cmd;
  match Unix.system cmd with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith ("Command failed: " ^ cmd)

let main () =
  Printf.printf "Starting main function\n%!";
  let tun = Tun.create "tun0" in
  Printf.printf "Created TUN interface\n%!";
  
  (* Configure IP and bring interface up *)
  run_command "ip addr add 10.0.0.1/24 dev tun0";
  run_command "ip link set tun0 up";
  Printf.printf "Configured tun0 with IP 10.0.0.1/24 and brought it up\n%!";
  
  let buf = Bytes.create 1500 in
  Printf.printf "Created buffer\n%!";
  while true do
    Printf.printf "\n[%s] Waiting for packet...\n%!" 
      (Unix.time () |> int_of_float |> string_of_int);
    let data = Tun.read tun buf in
    Printf.printf "Received packet of size %d bytes:\n" (Bytes.length data);
    hexdump data
  done

let () = main () 