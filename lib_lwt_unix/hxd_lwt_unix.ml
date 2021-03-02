open Hxd
module Lwt_scheduler = Make (struct type +'a t = 'a Lwt.t end)

type error = Lwt_unix of exn

let ( <.> ) f g x = f (g x)

let recv ic buffer ~off ~len =
  let res =
    Lwt.try_bind
      (fun () -> Lwt_unix.read ic buffer off len)
      (fun x -> Lwt.return_ok x)
      (fun exn -> Lwt.return_error (Lwt_unix exn)) in
  Lwt_scheduler.inj res

let send oc buffer ~off ~len =
  let res =
    Lwt.try_bind
      (fun () -> Lwt_unix.write oc (Bytes.unsafe_of_string buffer) off len)
      (fun x -> Lwt.return_ok x)
      (fun exn -> Lwt.return_error (Lwt_unix exn)) in
  Lwt_scheduler.inj res

let lwt_bind x f =
  let open Lwt.Infix in
  Lwt_scheduler.(inj (prj x >>= (prj <.> f)))

let lwt = {bind= lwt_bind; return= (fun x -> Lwt_scheduler.inj (Lwt.return x))}

let lseek =
  let lseek ic pos mode =
    let mode =
      match mode with
      | `CUR -> Lwt_unix.SEEK_CUR
      | `END -> Lwt_unix.SEEK_END
      | `SET -> Lwt_unix.SEEK_SET in
    let res =
      Lwt.try_bind
        (fun () -> Lwt_unix.lseek ic pos mode)
        (fun x -> Lwt.return_ok x)
        (fun exn -> Lwt.return_error (Lwt_unix exn)) in
    Lwt_scheduler.inj res in
  {lseek}

let generate configuration ic oc seek ppf =
  let res = generate configuration lwt recv send ic oc lseek seek ppf in
  Lwt_scheduler.prj res
