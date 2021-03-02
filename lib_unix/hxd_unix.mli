val generate :
     Hxd.cfg
  -> in_channel
  -> out_channel
  -> [ `Absolute of int | `Relative of int ]
  -> Format.formatter
  -> (unit, [ `Msg of string ]) result
