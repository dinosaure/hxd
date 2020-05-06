open Hxd
open S

module Lwt_scheduler = Make (struct
  type +'a t = 'a Lwt.t
end)

type error = |

let ( <.> ) f g x = f (g x)

let ok x = Ok x

module Lwt_iflow = struct
  type scheduler = Lwt_scheduler.t

  type nonrec error = error = |

  type flow = { mutable contents : unit -> (string * int * int) option Lwt.t }

  type buffer = bytes

  let input ic buffer ~off ~len =
    let open Lwt.Infix in
    let res =
      ic.contents () >>= function
      | None -> Lwt.return_ok 0
      | Some (res, off', len') ->
          let len'' = (min : int -> int -> int) len len' in
          Bytes.blit_string res off' buffer off len'' ;
          (if len'' < len'
          then
            (* XXX(dinosaure): deferred inputs. *)
            let consumed = ref false in
            ic.contents <-
              (fun () ->
                if !consumed
                then ic.contents ()
                else (
                  consumed := true ;
                  Lwt.return (Some (res, off' + len'', len' - len''))))) ;
          Lwt.return_ok len'' in
    Lwt_scheduler.inj res
end

module Lwt_oflow = struct
  type scheduler = Lwt_scheduler.t

  type nonrec error = error = |

  type flow = (string * int * int) option -> unit Lwt.t

  type buffer = string

  let output oc buffer ~off ~len =
    let open Lwt.Infix in
    let res = oc (Some (buffer, off, len)) >|= fun () -> ok len in
    Lwt_scheduler.inj res
end

let lwt_bind x f =
  let open Lwt.Infix in
  Lwt_scheduler.(inj (prj x >>= (prj <.> f)))

let lwt =
  { bind = lwt_bind; return = (fun x -> Lwt_scheduler.inj (Lwt.return x)) }

let lseek =
  {
    lseek =
      (fun _ pos mode ->
        let res =
          match (pos, mode) with
          | 0, `SET -> Lwt.return_ok 0
          | _, _ -> assert false in
        Lwt_scheduler.inj res);
  }

type input = unit -> (string * int * int) option Lwt.t

type output = (string * int * int) option -> unit Lwt.t

let o configuration ic oc ppf =
  let ic = { Lwt_iflow.contents = ic } in
  let res =
    O.o configuration lwt
      (module Lwt_iflow)
      (module Lwt_oflow)
      ic oc lseek (`Absolute 0) ppf in
  let open Lwt.Infix in
  Lwt_scheduler.prj res >>= function Ok () -> oc None | Error (_ : error) -> .
