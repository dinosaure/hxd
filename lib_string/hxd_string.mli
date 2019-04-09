val null : Format.formatter

val o :
  Hxd.O.configuration ->
  string ->
  [ `Absolute of int | `Relative of int ] ->
  Format.formatter ->
  (string, [ `Msg of string ]) result

val pp : Hxd.O.configuration -> Format.formatter -> string -> unit

val to_hxd :
  Hxd.O.configuration ->
  string ->
  [ `Absolute of int | `Relative of int ] ->
  (string, [ `Msg of string ]) result
