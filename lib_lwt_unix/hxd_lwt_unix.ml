open Hxd
open S

module Lwt_scheduler = Make(struct type +'a t = 'a Lwt.t end)

type error = Lwt_unix of exn

let ( <.> ) f g = fun x -> f (g x)

module Lwt_unix_iflow = struct
  type scheduler = Lwt_scheduler.t
  type nonrec error = error = Lwt_unix of exn
  type flow = Lwt_unix.file_descr
  type buffer = bytes

  let input ic buffer ~off ~len =
    let res =
      Lwt.try_bind
        (fun () -> Lwt_unix.read ic buffer off len)
        (fun x -> Lwt.return_ok x)
        (fun exn -> Lwt.return_error (Lwt_unix exn)) in
    Lwt_scheduler.inj res
end

module Lwt_unix_oflow = struct
  type scheduler = Lwt_scheduler.t
  type nonrec error = error = Lwt_unix of exn
  type flow = Lwt_unix.file_descr
  type buffer = string

  let output oc buffer ~off ~len =
    let res =
      Lwt.try_bind
        (fun () -> Lwt_unix.write oc (Bytes.unsafe_of_string buffer) off len)
        (fun x -> Lwt.return_ok x)
        (fun exn -> Lwt.return_error (Lwt_unix exn)) in
    Lwt_scheduler.inj res
end

let lwt_bind x f =
  let open Lwt.Infix in
  Lwt_scheduler.(inj (prj x >>= (prj <.> f)))

let lwt =
  { bind= lwt_bind
  ; return= (fun x -> Lwt_scheduler.inj (Lwt.return x)) }

let lseek =
  { lseek= fun ic pos mode ->
        let mode = match mode with
          | `CUR -> Lwt_unix.SEEK_CUR
          | `END -> Lwt_unix.SEEK_END
          | `SET -> Lwt_unix.SEEK_SET in
        let res =
          Lwt.try_bind
            (fun () -> Lwt_unix.lseek ic pos mode)
            (fun x -> Lwt.return_ok x)
            (fun exn -> Lwt.return_error (Lwt_unix exn)) in
        Lwt_scheduler.inj res }

let o configuration ic oc seek ppf =
  let res = O.o configuration lwt (module Lwt_unix_iflow) (module Lwt_unix_oflow) ic oc lseek seek ppf in
  Lwt_scheduler.prj res
