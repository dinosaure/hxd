open Hxd
open S

module Caml_scheduler = Make(struct type +'a t = 'a end)

type error = Seek

module type SINGLETON = sig type t val v : t end
type flow = { mutable off : int; len : int }

module Caml_iflow (X : SINGLETON with type t = string) = struct
  type scheduler = Caml_scheduler.t
  type nonrec error = error = Seek
  type nonrec flow = flow = { mutable off : int; len : int }
  type buffer = bytes

  let input flow buffer ~off ~len =
    let len = (min : int -> int -> int) len (flow.len - flow.off) in
    Bytes.blit_string X.v flow.off buffer off len ; flow.off <- flow.off + len ; Caml_scheduler.inj (Ok len)
end

module Caml_oflow = struct
  type scheduler = Caml_scheduler.t
  type nonrec error = error = Seek
  type flow = Buffer.t
  type buffer = string

  let output buf buffer ~off ~len =
    Buffer.add_substring buf buffer off len ; Caml_scheduler.inj (Ok len)
end

let caml =
  { bind= (fun x f -> f (Caml_scheduler.prj x))
  ; return= Caml_scheduler.inj }

let lseek =
  { lseek= fun flow pos mode ->
        let () = match mode with
          | `CUR -> flow.off <- flow.off + pos
          | `SET -> flow.off <- pos
          | `END -> flow.off <- flow.len - pos in
        if flow.off < 0 || flow.off >= flow.len
        then Caml_scheduler.inj (Error Seek)
        else Caml_scheduler.inj (Ok flow.off) }

let o configuration str seek ppf =
  let module Caml_iflow = Caml_iflow(struct type t = string let v = str end) in
  let ic = { off= 0; len= String.length str } in
  let oc = Buffer.create 16 in
  let res = O.o configuration caml (module Caml_iflow) (module Caml_oflow) ic oc lseek seek ppf in
  match Caml_scheduler.prj res with
  | Ok () -> Ok (Buffer.contents oc)
  | Error Seek -> Error (`Msg "Index out of bounds")

module Caml_onull = struct
  type scheduler = Caml_scheduler.t
  type nonrec error = error = Seek
  type flow = ()
  type buffer = string

  let output () _ ~off:_ ~len = Caml_scheduler.inj (Ok len)
end

let pp configuration ppf str =
  let module Caml_iflow = Caml_iflow(struct type t = string let v = str end) in
  let ic = { off= 0; len= String.length str } in
  let seek = `Absolute 0 in
  let res = O.o configuration caml (module Caml_iflow) (module Caml_onull) ic () lseek seek ppf in
  match Caml_scheduler.prj res with
  | Ok () -> ()
  | Error Seek -> ()
    (* XXX(dinosaure): nothing to do where [String.length str = 0] *)

let null =
  Format.formatter_of_out_functions
    { Format.out_string= (fun _ _ _ -> ())
    ; out_flush= (fun () -> ())
    ; out_newline= (fun () -> ())
    ; out_spaces= (fun _ -> ())
    ; out_indent= (fun _ -> ()) }

let to_hxd configuration str seek =
  o configuration str seek null


