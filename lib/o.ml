open S

type colorscheme = Fmt.style array

let colorscheme_of_array : Fmt.style array -> colorscheme =
 fun x ->
  if Array.length x <> 256 then
    Fmt.invalid_arg "colorscheme_of_array: expect 256 elements"
  ; x

let lowercase x color =
  for i = 97 to 122 do
    x.(i) <- color
  done

let uppercase x color =
  for i = 65 to 90 do
    x.(i) <- color
  done

let digit x color =
  for i = 48 to 57 do
    x.(i) <- color
  done

let code x i color = x.(i) <- color

type xxd = {
    cols: int
  ; groupsize: int
  ; long: int option
  ; uppercase: bool
  ; i_buffer_size: int
  ; o_buffer_size: int
  ; colorscheme: colorscheme
}

type caml = {
    kind: [ `List | `Array ]
  ; with_comments: bool
  ; cols: int
  ; long: int option
  ; i_buffer_size: int
  ; o_buffer_size: int
  ; uppercase: bool
}

type cfg = Xxd of xxd | Caml of caml

let ( .!{} ) str pos = Char.code str.[pos]
let ( .![]<- ) bytes pos chr = Bytes.set bytes pos chr

let to_hexdigit alphabet pad off output len value =
  let base = String.length alphabet in
  let cursor = ref (len - 1) in
  let value = ref value in
  while
    let code = alphabet.[!value mod base] in
    Bytes.set output (off + !cursor) code
    ; value := !value / base
    ; decr cursor
    ; !cursor >= 0 && !value <> 0
  do
    ()
  done
  ; while !cursor >= 0 do
      Bytes.set output (off + !cursor) pad
      ; decr cursor
    done

let apply_style code ~off output = Fmt.ansi_style_code output off code
let reset_style ~off output = Fmt.ansi_style_code output off `None
let _begin = 0b01
let _end = 0b10

let with_comments (cfg : caml) input src_off src_len output dst_off =
  let dst_off =
    if src_len < cfg.cols then begin
      for i = 0 to cfg.cols - src_len - 1 do
        output.![dst_off + (i * 4) + 0] <- ' '
        ; output.![dst_off + (i * 4) + 1] <- ' '
        ; output.![dst_off + (i * 4) + 2] <- ' '
        ; output.![dst_off + (i * 4) + 3] <- ' '
      done
      ; dst_off + ((cfg.cols - src_len) * 4)
    end
    else dst_off in
  output.![dst_off + 0] <- ' '
  ; output.![dst_off + 1] <- '('
  ; output.![dst_off + 2] <- '*'
  ; output.![dst_off + 3] <- ' '
  ; let cursor = ref 0 in
    let dst_off = ref (dst_off + 4) in
    while !cursor < src_len do
      (match input.[src_off + !cursor] with
      | '\032' .. '\126' as chr -> output.![!dst_off] <- chr
      | _ -> output.![!dst_off] <- '.')
      ; incr dst_off ; incr cursor
    done
    ; while !cursor < cfg.cols do
        output.![!dst_off] <- ' ' ; incr dst_off ; incr cursor
      done
    ; output.![!dst_off + 0] <- ' '
    ; output.![!dst_off + 1] <- '*'
    ; output.![!dst_off + 2] <- ')'
    ; !dst_off + 3

let a_uppercase = "0123456789ABCDEF"
let a_lowercase = "0123456789abcdef"
let length_of_decimal n = if n >= 100 then 3 else if n >= 10 then 2 else 1

let deterministic_length_of_style = function
  | `None -> 3
  | `Style (_, `bit24 (r, g, b)) ->
    7
    + length_of_decimal r
    + 1
    + length_of_decimal g
    + 1
    + length_of_decimal b
    + 1
  | `Style (_, #Fmt.rest) -> 0

(* TODO *)

let deterministic_length styled (cfg : xxd) input off len =
  match styled with
  | true ->
    let res = ref 0 in
    for i = 0 to cfg.cols - 1 do
      if i mod cfg.groupsize = 0 then incr res
      ; if i < len then
          res :=
            !res
            + deterministic_length_of_style cfg.colorscheme.(input.!{off + i})
            + 2
            + 3
        else res := !res + 2
    done
    ; !res + 2
  | false ->
    let res = ref 0 in
    for i = 0 to cfg.cols - 1 do
      if i mod cfg.groupsize = 0 then incr res
      ; res := !res + 2
    done
    ; !res + 2

let formatter_is_styled ppf =
  match Fmt.style_renderer ppf with `None -> false | `Ansi -> true

let to_line cfg ppf ~seek ?(state = 0) input ~src_off ~src_len output ~dst_off =
  let styled = formatter_is_styled ppf in
  let anchor = dst_off in
  match cfg with
  | Xxd cfg ->
    let alphabet = if cfg.uppercase then a_uppercase else a_lowercase in
    to_hexdigit alphabet '0' dst_off output 8 seek
    ; output.![dst_off + 8] <- ':'
    ; (* output.![dst_off + 9] <- ' ' ; *)
      let off = ref (dst_off + 9) in
      let top = !off + deterministic_length styled cfg input src_off src_len in
      for i = 0 to src_len - 1 do
        if i mod cfg.groupsize = 0 then (output.![!off] <- ' ' ; incr off)
        ; if styled then
            off :=
              apply_style
                cfg.colorscheme.(input.!{src_off + i})
                ~off:!off output
        ; to_hexdigit alphabet '0' !off output 2 input.!{src_off + i}
        ; off := !off + 2
        ; if styled then off := reset_style ~off:!off output
      done
      ; for _ = !off to top - 1 do
          output.![!off] <- ' ' ; incr off
        done
      ; for i = 0 to src_len - 1 do
          match input.[src_off + i] with
          | '\032' .. '\126' as chr -> output.![!off + i] <- chr
          | _ -> output.![!off + i] <- '.'
        done
      ; Format.fprintf ppf "%s@,"
          (Bytes.sub_string output anchor (!off + src_len - anchor))
      ; output.![!off + src_len] <- '\n'
      ; !off + src_len + 1
  | Caml cfg ->
    let alphabet = if cfg.uppercase then a_uppercase else a_lowercase in
    let off = ref dst_off in
    (if state land _begin <> 0 then (
     match cfg.kind with
     | `List -> output.![!off] <- '[' ; incr off
     | `Array ->
       output.![!off] <- '['
       ; output.![!off + 1] <- '|'
       ; off := !off + 2)
    else
      match cfg.kind with
      | `List -> output.![!off] <- ';' ; incr off
      | `Array ->
        output.![!off] <- ' '
        ; output.![!off + 1] <- ';'
        ; off := !off + 2)
    ; output.![!off] <- ' '
      ; output.![!off + 1] <- '"'
      ; off := !off + 2
      ; for i = 0 to src_len - 1 do
          output.![!off] <- '\\'
          ; incr off
          ; output.![!off] <- 'x'
          ; incr off
          ; to_hexdigit alphabet '0' !off output 2 input.!{src_off + i}
          ; off := !off + 2
        done
      ; output.![!off] <- '"'
      ; incr off
      ; (if state land _end <> 0 then (
         match cfg.kind with
         | `List ->
           output.![!off] <- ' '
           ; output.![!off + 1] <- ']'
           ; off := !off + 2
         | `Array ->
           output.![!off] <- ' '
           ; output.![!off + 1] <- '|'
           ; output.![!off + 2] <- ']'
           ; off := !off + 3)
        else if cfg.with_comments then
          match cfg.kind with
          | `List ->
            output.![!off] <- ' '
            ; output.![!off + 1] <- ' '
            ; off := !off + 2
          | `Array ->
            output.![!off] <- ' '
            ; output.![!off + 1] <- ' '
            ; output.![!off + 2] <- ' '
            ; off := !off + 3)
      ; if cfg.with_comments then
          off := with_comments cfg input src_off src_len output !off
        ; output.![!off] <- '\n'
        ; !off + 1

let xxd ?(cols = 16) ?(groupsize = 2) ?long ?(uppercase = false) colorscheme =
  let i_buffer_size = cols in
  let spaces = cols / groupsize in
  let digits = (3 + 19) * 2 * cols in
  let header = 8 + 1 in
  let ascii = 2 + (cols * (3 + 19)) in
  let o_buffer_size = header + spaces + digits + 2 + ascii + 1 in
  Xxd
    {
      cols
    ; groupsize
    ; long
    ; uppercase
    ; i_buffer_size
    ; o_buffer_size
    ; colorscheme
    }

let none = Array.make 256 `None
let default = xxd none

let caml ?(with_comments = false) ?(cols = 16) ?long ?(uppercase = false) kind =
  let i_buffer_size = cols in
  let o_buffer_size = 3 + 1 + (cols * 4) + 1 + 3 in
  let o_buffer_size =
    match with_comments with
    | false -> o_buffer_size
    | true -> o_buffer_size + 7 + cols in
  Caml
    {kind; with_comments; cols; long; i_buffer_size; o_buffer_size; uppercase}

let cols = function Xxd xxd -> xxd.cols | Caml caml -> caml.cols
let long = function Xxd xxd -> xxd.long | Caml caml -> caml.long
let io_buffer_size = 4096

let refill :
    type fi s e.
       s scheduler
    -> (fi, bytes, s, e) input
    -> fi
    -> src_off:int
    -> ?max:int
    -> bytes
    -> ((bool * int, e) result, s) io =
 fun s recv ic ~src_off ?max buffer ->
  let ( >>= ) = s.bind in
  let return = s.return in
  let len =
    match max with
    | Some max -> min max (Bytes.length buffer - src_off)
    | None -> Bytes.length buffer - src_off in

  if len = 0 then return (Ok (true, 0))
  else
    recv ic buffer ~off:src_off ~len >>= function
    | Error err -> return (Error err)
    | Ok 0 -> return (Ok (true, 0))
    | Ok len -> return (Ok (false, len))

let flush :
    type fo s e.
       s scheduler
    -> (fo, string, s, e) output
    -> fo
    -> string
    -> len:int
    -> ((int, e) result, s) io =
 fun _ send oc str ~len -> send oc str ~off:0 ~len

let flush_all :
    type fo s e.
       s scheduler
    -> (fo, string, s, e) output
    -> fo
    -> string
    -> len:int
    -> ((unit, e) result, s) io =
 fun s send oc str ~len:max ->
  let ( >>= ) = s.bind in
  let return = s.return in

  let rec go cursor =
    let len = max - cursor in
    send oc str ~off:cursor ~len >>= function
    | Ok len -> if cursor + len = max then return (Ok ()) else go (cursor + len)
    | Error err -> return (Error err) in
  go 0

let i_buffer_size = function
  | Xxd {i_buffer_size; _} -> i_buffer_size
  | Caml {i_buffer_size; _} -> i_buffer_size

let o_buffer_size = function
  | Xxd {o_buffer_size; _} -> o_buffer_size
  | Caml {o_buffer_size; _} -> o_buffer_size

let sub len max = max - len
let option_map f = function Some x -> Some (f x) | None -> None

let generate :
    type fi fo s e.
       cfg
    -> s scheduler
    -> (fi, bytes, s, e) input
    -> (fo, string, s, e) output
    -> fi
    -> fo
    -> (fi, s, e) seek
    -> [ `Absolute of int | `Relative of int ]
    -> Format.formatter
    -> ((unit, e) result, s) io =
 fun cfg s recv send ic oc {lseek} seek ppf ->
  let ( >>= ) = s.bind in
  let return = s.return in
  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> return (Error err) in

  let rec go state seek input src_off src_len output dst_off max =
    if src_len > 0 then Bytes.blit input src_off input 0 src_len
    ; refill s recv ic ~src_off:src_len input ?max >>? function
      | false, len ->
        let max = option_map (sub len) max in
        let src_len = src_len + len in
        let lines =
          min
            ((src_len / i_buffer_size cfg) - 1)
            ((io_buffer_size - dst_off) / o_buffer_size cfg) in
        let src_off = ref 0 in
        let dst_off = ref dst_off in
        let seek = ref seek in
        let state = ref state in
        for _ = 0 to lines - 1 do
          dst_off :=
            to_line cfg ppf ~state:!state ~seek:!seek
              (Bytes.unsafe_to_string input)
              ~src_off:!src_off ~src_len:(i_buffer_size cfg) output
              ~dst_off:!dst_off
          ; seek := !seek + cols cfg
          ; state := !state land _end
          ; src_off := !src_off + cols cfg
        done
        ; flush s send oc (Bytes.unsafe_to_string output) ~len:!dst_off
          >>? fun len ->
          let rem = !dst_off - len in
          if rem > 0 then Bytes.blit output len output 0 rem
          ; go !state !seek input !src_off (src_len - !src_off) output rem max
      | true, len ->
        let src_len = src_len + len in
        let lines =
          min
            (src_len / i_buffer_size cfg)
            ((io_buffer_size - dst_off) / o_buffer_size cfg) in
        let lines =
          if
            src_len mod i_buffer_size cfg > 0
            && succ lines * o_buffer_size cfg <= io_buffer_size - dst_off
          then lines + 1
          else lines in
        let src_off = ref 0 in
        let dst_off = ref dst_off in
        let seek = ref seek in
        let state = ref state in
        for _ = 0 to lines - 1 do
          state :=
            !state lor if src_len - !src_off <= cols cfg then _end else 0b0
          ; let len = min (src_len - !src_off) (i_buffer_size cfg) in
            dst_off :=
              to_line cfg ppf ~state:!state ~seek:!seek
                (Bytes.unsafe_to_string input)
                ~src_off:!src_off ~src_len:len output ~dst_off:!dst_off
            ; seek := !seek + len
            ; src_off := !src_off + len
            ; state := !state land _end
        done
        ; flush_all s send oc (Bytes.unsafe_to_string output) ~len:!dst_off
          >>? fun () ->
          if !src_off - src_len = 0 then return (Ok ())
          else pending !state !seek input !src_off src_len output
  and pending state seek input src_off src_len output =
    let lines =
      min
        ((src_len - src_off) / i_buffer_size cfg)
        (io_buffer_size / o_buffer_size cfg) in
    let lines =
      if
        (src_len - src_off) mod i_buffer_size cfg > 0
        && succ lines * o_buffer_size cfg <= io_buffer_size
      then lines + 1
      else lines in
    let src_off = ref src_off in
    let dst_off = ref 0 in
    let seek = ref seek in
    let state = ref state in
    for _ = 0 to lines - 1 do
      state := !state lor if src_len - !src_off <= cols cfg then _end else 0b0
      ; let len = min (src_len - !src_off) (i_buffer_size cfg) in
        dst_off :=
          to_line cfg ppf ~state:!state ~seek:!seek
            (Bytes.unsafe_to_string input)
            ~src_off:!src_off ~src_len:len output ~dst_off:!dst_off
        ; seek := !seek + len
        ; src_off := !src_off + len
        ; state := !state land _end
    done
    ; flush_all s send oc (Bytes.unsafe_to_string output) ~len:!dst_off
      >>? fun () ->
      if !src_off - src_len = 0 then return (Ok ())
      else pending !state !seek input !src_off src_len output in

  match seek with
  | `Absolute pos ->
    (if pos < 0 then lseek ic (-pos) `END else lseek ic pos `SET)
    >>? fun seek ->
    go _begin seek
      (Bytes.create io_buffer_size)
      0 0
      (Bytes.create io_buffer_size)
      0 (long cfg)
  | `Relative 0 ->
    go _begin 0
      (Bytes.create io_buffer_size)
      0 0
      (Bytes.create io_buffer_size)
      0 (long cfg)
  | `Relative pos ->
    lseek ic pos `CUR >>? fun seek ->
    go _begin seek
      (Bytes.create io_buffer_size)
      0 0
      (Bytes.create io_buffer_size)
      0 (long cfg)
