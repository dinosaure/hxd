open Hxd
open S

module Unix_scheduler = Make(struct type +'a t = 'a end)

type error = Seek

module Caml_iflow = struct
  type scheduler = Unix_scheduler.t
  type nonrec error = error = Seek
  type flow = in_channel
  type buffer = bytes

  let input ic buffer ~off ~len = let len = input ic buffer off len in Unix_scheduler.inj (Ok len)
end

module Caml_oflow = struct
  type scheduler = Unix_scheduler.t
  type nonrec error = error = Seek
  type flow = out_channel
  type buffer = string

  let output oc buffer ~off ~len = output oc (Bytes.unsafe_of_string buffer) off len ; Unix_scheduler.inj (Ok len)
end

let unix =
  { bind= (fun x f -> f (Unix_scheduler.prj x))
  ; return= Unix_scheduler.inj }

let lseek =
  { lseek= fun ic pos mode ->
        let mode = match mode with
          | `CUR -> Unix.SEEK_CUR
          | `END -> Unix.SEEK_END
          | `SET -> Unix.SEEK_SET in
        match Unix.lseek (Unix.descr_of_in_channel ic) pos mode with
        | res -> Unix_scheduler.inj (Ok res)
        | exception _ -> Unix_scheduler.inj (Error Seek)}

let o configuration ic oc seek ppf =
  let res = O.o configuration unix (module Caml_iflow) (module Caml_oflow) ic oc lseek seek ppf in
  match Unix_scheduler.prj res with
  | Ok () -> Ok ()
  | Error Seek -> Error (`Msg "Seek operation")
