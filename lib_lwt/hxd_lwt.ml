open Hxd
module Lwt_scheduler = Make (struct type +'a t = 'a Lwt.t end)

type error = |

let ( <.> ) f g x = f (g x)
let ok x = Ok x

let lwt_bind x f =
  let open Lwt.Infix in
  Lwt_scheduler.(inj (prj x >>= (prj <.> f)))

let lwt = {bind= lwt_bind; return= (fun x -> Lwt_scheduler.inj (Lwt.return x))}

let lseek =
  let lseek _ pos mode =
    let res =
      match pos, mode with 0, `SET -> Lwt.return_ok 0 | _, _ -> assert false
    in
    Lwt_scheduler.inj res in
  {lseek}

type input = unit -> (string * int * int) option Lwt.t
type output = (string * int * int) option -> unit Lwt.t

let recv ic buffer ~off ~len =
  let open Lwt.Infix in
  let res =
    ic.contents () >>= function
    | None -> Lwt.return_ok 0
    | Some (res, off', len') ->
      let len'' = (min : int -> int -> int) len len' in
      Bytes.blit_string res off' buffer off len''
      ; (if len'' < len' then
         (* XXX(dinosaure): deferred inputs. *)
         let consumed = ref false in
         ic.contents <-
           (fun () ->
             if !consumed then ic.contents ()
             else (
               consumed := true
               ; Lwt.return (Some (res, off' + len'', len' - len'')))))
      ; Lwt.return_ok len'' in
  Lwt_scheduler.inj res

let send oc buffer ~off ~len =
  let open Lwt.Infix in
  let res = oc (Some (buffer, off, len)) >|= fun () -> ok len in
  Lwt_scheduler.inj res

let generate configuration ic oc ppf =
  let ic = {contents= ic} in
  let res = generate configuration lwt recv send ic oc lseek (`Absolute 0) ppf in
  let open Lwt.Infix in
  Lwt_scheduler.prj res >>= function Ok () -> oc None | Error (_ : error) -> .
