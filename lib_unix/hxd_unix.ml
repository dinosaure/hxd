open Hxd
module Unix_scheduler = Make (struct type +'a t = 'a end)

type error = Seek

let unix =
  {bind= (fun x f -> f (Unix_scheduler.prj x)); return= Unix_scheduler.inj}

let lseek_in_file =
  let lseek ic pos mode =
    let mode =
      match mode with
      | `CUR -> Unix.SEEK_CUR
      | `END -> Unix.SEEK_END
      | `SET -> Unix.SEEK_SET in
    match Unix.lseek (Unix.descr_of_in_channel ic) pos mode with
    | res -> Unix_scheduler.inj (Ok res)
    | exception _exn -> Unix_scheduler.inj (Error Seek) in
  {lseek}

let drain fd max =
  let tmp = Bytes.create 0x1000 in
  let rec go cursor =
    let len = min 0x1000 (max - cursor) in
    let res = Unix.read fd tmp 0 len in
    if res = 0 then raise End_of_file
    ; if cursor + res < max then go (cursor + res) in
  go 0

let lseek_in_stdin =
  let lseek ic pos mode =
    (* XXX(dinosaure): we must must know where we are in [stdin]. *)
    let cur = try Unix.lseek Unix.stdin 0 SEEK_CUR with _exn -> 0 in
    match mode with
    | `CUR when pos >= 0 -> (
      try
        drain (Unix.descr_of_in_channel ic) pos
        ; Unix_scheduler.inj (Ok (cur + pos))
      with End_of_file -> Unix_scheduler.inj (Error Seek))
    | `SET when pos >= 0 && cur = 0 -> (
      try
        drain (Unix.descr_of_in_channel ic) pos
        ; Unix_scheduler.inj (Ok (cur + pos))
      with End_of_file -> Unix_scheduler.inj (Error Seek))
    | `CUR when pos < 0 ->
      let pos = Unix.lseek Unix.stdin pos SEEK_CUR in
      Unix_scheduler.inj (Ok pos)
    | `SET ->
      let pos = Unix.lseek Unix.stdin pos SEEK_SET in
      Unix_scheduler.inj (Ok pos)
    | _ -> Unix_scheduler.inj (Error Seek) in
  {lseek}

let recv ic buffer ~off ~len =
  let len = input ic buffer off len in
  Unix_scheduler.inj (Ok len)

let send oc buffer ~off ~len =
  output oc (Bytes.unsafe_of_string buffer) off len
  ; Unix_scheduler.inj (Ok len)

let generate configuration ic oc seek ppf =
  let lseek = if ic == stdin then lseek_in_stdin else lseek_in_file in
  let res = generate configuration unix recv send ic oc lseek seek ppf in
  match Unix_scheduler.prj res with
  | Ok () -> Ok ()
  | Error Seek -> Error (`Msg "sorry cannot seek")
