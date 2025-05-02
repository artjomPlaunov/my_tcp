open Utils  (* Assumes you have Tun.create and run_command defined *)

let read_packet filename =
  let in_channel = open_in_bin filename in
  let packet = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;
  Bytes.of_string packet  (* Convert the string to bytes *)

let send_packet packet tun =
  
  (* Send the packet *)
  let _ = Tun.write tun packet in
  Printf.printf "Sent raw packet\n%!"

let main () =
  (* Set up the tunnel interface *)
  let tun = Tun.create "tun1" in
  run_command "sudo ip addr add 10.0.1.1/24 dev tun1";
  run_command "sudo ip link set tun1 up";

  (* Read the packet from file *)
  let packet = read_packet "packet.bin" in
  
  (* Loop to repeatedly send the packet *)
  Printf.printf "Sending packet repeatedly. Press Ctrl+C to stop.\n%!";
  while true do
    send_packet packet tun;
    Unix.sleep 1  (* Sleep for 1 second before sending the packet again *)
  done

let () = main ()
