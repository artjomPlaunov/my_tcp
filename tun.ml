type t = {
  fd : Unix.file_descr;
  name : string;
}

let create name =
  Printf.printf "Tun.create: starting with name %s\n%!" name;
  let fd, actual_name = Tuntap.opentun ~devname:name () in
  Printf.printf "Tun.create: got fd and name %s\n%!" actual_name;
  Unix.clear_nonblock fd;
  Printf.printf "Tun.create: cleared nonblock\n%!";
  { fd; name = actual_name }

let close t = Unix.close t.fd

let rec read_n t buf offset n =
  if n <= 0 then n
  else
    let len = Unix.read t.fd buf offset n in
    if len = 0 then raise End_of_file;
    Printf.printf "Read %d bytes at offset %d (total buffer size: %d)\n" len offset n;
    if len < n then begin
      Printf.printf "Received complete packet of size %d bytes\n" (offset + len);
      offset + len
    end else
      read_n t buf (offset + len) (n - len)

let read t buf =
  let len = read_n t buf 0 (Bytes.length buf) in
  Bytes.sub buf 0 len

let write t buf =
  let len = Unix.write t.fd buf 0 (Bytes.length buf) in
  len 