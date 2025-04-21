type t = {
  fd : Unix.file_descr;
  name : string;
}

let create name =
  let fd, actual_name = Tuntap.opentun ~devname:name () in
  Unix.clear_nonblock fd;
  { fd; name = actual_name }

let close t = Unix.close t.fd

let rec read_n t buf offset n =
  if n <= 0 then n
  else
    let len = Unix.read t.fd buf offset n in
    if len = 0 then raise End_of_file;
    if len < n then begin
      offset + len
    end else
      read_n t buf (offset + len) (n - len)

let read t buf =
  let len = read_n t buf 0 (Bytes.length buf) in
  Bytes.sub buf 0 len

let write t buf =
  let len = Unix.write t.fd buf 0 (Bytes.length buf) in
  len 