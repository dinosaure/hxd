open Hxd
module Caml_scheduler = Make (struct type +'a t = 'a end)

type error = Seek
type flow = {mutable off: int; len: int}

let caml =
  {bind= (fun x f -> f (Caml_scheduler.prj x)); return= Caml_scheduler.inj}

let lseek =
  let lseek flow pos mode =
    let () =
      match mode with
      | `CUR -> flow.off <- flow.off + pos
      | `SET -> flow.off <- pos
      | `END -> flow.off <- flow.len - pos in
    if flow.off < 0 || flow.off >= flow.len then Caml_scheduler.inj (Error Seek)
    else Caml_scheduler.inj (Ok flow.off) in
  {lseek}

let pp configuration ppf str =
  let ic = {off= 0; len= String.length str} in
  let recv flow buffer ~off ~len =
    let len = min len (flow.len - flow.off) in
    Bytes.blit_string str flow.off buffer off len
    ; flow.off <- flow.off + len
    ; Caml_scheduler.inj (Ok len) in
  let send _ _ ~off:_ ~len = Caml_scheduler.inj (Ok len) in
  let seek = `Absolute 0 in
  let res = generate configuration caml recv send ic () lseek seek ppf in
  match Caml_scheduler.prj res with Ok () -> () | Error Seek -> ()

let generate configuration str seek ppf =
  let ic = {off= 0; len= String.length str} in
  let oc = Buffer.create 16 in
  let recv flow buffer ~off ~len =
    let len = min len (flow.len - flow.off) in
    Bytes.blit_string str flow.off buffer off len
    ; flow.off <- flow.off + len
    ; Caml_scheduler.inj (Ok len) in
  let send buf buffer ~off ~len =
    Buffer.add_substring buf buffer off len
    ; Caml_scheduler.inj (Ok len) in
  let res = generate configuration caml recv send ic oc lseek seek ppf in
  match Caml_scheduler.prj res with
  | Ok () -> Ok (Buffer.contents oc)
  | Error Seek -> Error (`Msg "Index out of bounds")

let null =
  Format.formatter_of_out_functions
    {
      Format.out_string= (fun _ _ _ -> ())
    ; out_flush= (fun () -> ())
    ; out_newline= (fun () -> ())
    ; out_spaces= (fun _ -> ())
    ; out_indent= (fun _ -> ())
    }

let to_hxd configuration str seek = generate configuration str seek null
