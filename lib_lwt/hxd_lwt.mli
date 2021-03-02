type input = unit -> (string * int * int) option Lwt.t
type output = (string * int * int) option -> unit Lwt.t

val generate : Hxd.cfg -> input -> output -> Format.formatter -> unit Lwt.t
