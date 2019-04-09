open S

type colorscheme

val colorscheme_of_array : Fmt.style array -> colorscheme
val lowercase : colorscheme -> Fmt.style -> unit
val uppercase : colorscheme -> Fmt.style -> unit
val digit : colorscheme -> Fmt.style -> unit
val code : colorscheme -> int -> Fmt.style -> unit

type configuration

val configuration :
  ?cols:int ->
  ?groupsize:int ->
  ?long:int ->
  ?uppercase:bool ->
  colorscheme ->
  configuration

val default : configuration

val o :
  configuration ->
  's scheduler ->
  ('fi, bytes, 's, 'e) iflow ->
  ('fo, string, 's, 'e) oflow ->
  'fi -> 'fo ->
  ('fi, 's, 'e) seek ->
  [ `Absolute of int | `Relative of int ] ->
  Format.formatter ->
  ((unit, 'e) result, 's) io
