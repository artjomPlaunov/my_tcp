(* background.ml *)

open Eio

let main env =
  Switch.run (fun sw ->
    (* Spawn a background fiber *)
    Fiber.fork ~sw (fun () ->
      let clock = Stdenv.clock env in
      for i = 1 to 5 do
        traceln "Background fiber: iteration %d" i;
        Time.sleep clock 1.0
      done;
      traceln "Background fiber: done"
    );

    (* Main fiber continues concurrently *)
    traceln "Main fiber: running";
    Time.sleep (Stdenv.clock env) 3.0;
    traceln "Main fiber: done"
  )

let () = Eio_main.run main
