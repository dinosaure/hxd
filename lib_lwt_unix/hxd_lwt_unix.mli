type error = Lwt_unix of exn

val o :
  Hxd.O.cfg ->
  Lwt_unix.file_descr ->
  Lwt_unix.file_descr ->
  [ `Absolute of int | `Relative of int ] ->
  Format.formatter ->
  (unit, error) result Lwt.t
