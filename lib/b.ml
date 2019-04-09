type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external bigstring_get : bigstring -> int -> int = "%caml_ba_ref_1"
external bigstring_set : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external bigstring_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"
external bigstring_set_uint32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32u"

external string_get : string -> int -> int = "%string_unsafe_get"
external string_get_uint32 : string -> int -> int32 = "%caml_string_get32u"

external bytes_get : bytes -> int -> int = "%bytes_unsafe_get"
external bytes_set : bytes -> int -> int -> unit = "%bytes_unsafe_set"
external bytes_get_uint32 : bytes -> int -> int32 = "%caml_string_get32u"
external bytes_set_uint32 : bytes -> int -> int32 -> unit = "%caml_string_set32u"

(* string -> bigstring *)

let slow_blit_of_string src src_off dst dst_off len =
  for i = 0 to len - 1
  do bigstring_set dst (dst_off + i) (string_get src (src_off + i)) done

let blit_of_string src src_off dst dst_off len =
  if len < 4
  then slow_blit_of_string src src_off dst dst_off len
  else
    let len0 = len land 3 in
    let len1 = len asr 2 in

    for i = 0 to len1 - 1
    do
      let i = i * 4 in
      bigstring_set_uint32 dst (dst_off + i) (string_get_uint32 src (src_off + i))
    done ;

    for i = 0 to len0 - 1
    do
      let i = len1 * 4 + i in
      bigstring_set dst (dst_off + i) (string_get src (src_off + i))
    done

(* bytes -> bigstring *)

let slow_blit_of_bytes src src_off dst dst_off len =
  for i = 0 to len - 1
  do bigstring_set dst (dst_off + i) (bytes_get src (src_off + i)) done

let blit_of_bytes src src_off dst dst_off len =
  if len < 4
  then slow_blit_of_bytes src src_off dst dst_off len
  else
    let len0 = len land 3 in
    let len1 = len asr 2 in

    for i = 0 to len1 - 1
    do
      let i = i * 4 in
      bigstring_set_uint32 dst (dst_off + i) (bytes_get_uint32 src (src_off + i))
    done ;

    for i = 0 to len0 - 1
    do
      let i = len1 * 4 + i in
      bigstring_set dst (dst_off + i) (bytes_get src (src_off + i))
    done

(* bigstring -> bytes *)

let slow_blit_to_bytes src src_off dst dst_off len =
  for i = 0 to len - 1
  do bytes_set dst (dst_off + i) (bigstring_get src (src_off + i)) done

let blit_to_bytes src src_off dst dst_off len =
  if len < 4
  then slow_blit_to_bytes src src_off dst dst_off len
  else
    let len0 = len land 3 in
    let len1 = len asr 2 in

    for i = 0 to len1 - 1
    do
      let i = i * 4 in
      bytes_set_uint32 dst (dst_off + i) (bigstring_get_uint32 src (src_off + i))
    done ;

    for i = 0 to len0 - 1
    do
      let i = len1 * 4 + i in
      bytes_set dst (dst_off + i) (bigstring_get src (src_off + i))
    done

