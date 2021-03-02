val null : Format.formatter

val generate :
     Hxd.cfg
  -> string
  -> [ `Absolute of int | `Relative of int ]
  -> Format.formatter
  -> (string, [ `Msg of string ]) result

val pp : Hxd.cfg -> Format.formatter -> string -> unit

val to_hxd :
     Hxd.cfg
  -> string
  -> [ `Absolute of int | `Relative of int ]
  -> (string, [ `Msg of string ]) result
