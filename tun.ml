type t = { fd : Unix.file_descr; name : string }

let create name =
  let fd, actual_name = Tuntap.opentun ~pi:false ~devname:name () in
  Unix.clear_nonblock fd;
  { fd; name = actual_name }

let close t = Unix.close t.fd
let read t buf = Unix.read t.fd buf 0 (Bytes.length buf)

let write t buf =
  let len = Unix.write t.fd buf 0 (Bytes.length buf) in
  len
