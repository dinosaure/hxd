open S

module Option = struct
  let map_value ~default f = function
    | Some x -> f x
    | None -> default
end

type colorscheme = Fmt.style array

let colorscheme_of_array : Fmt.style array -> colorscheme = fun x ->
  if Array.length x <> 256 then Fmt.invalid_arg "Hxd.O.colorscheme_of_array: expect 256 elements" ; x

let lowercase x color =
  for i = 97 to 122 do x.(i) <- color done
let uppercase x color =
  for i = 65 to 90 do x.(i) <- color done
let digit x color =
  for i = 48 to 57 do x.(i) <- color done
let code x i color = x.(i) <- color

type payload = string

type xxd =
  { cols : int
  ; groupsize : int
  ; long : int option
  ; uppercase : bool
  ; i_buffer_size : int
  ; o_buffer_size : int
  ; colorscheme : colorscheme }

type caml =
  { kind : [ `List | `Array ]
  ; with_comments : bool
  ; cols : int
  ; long : int option
  ; i_buffer_size : int
  ; o_buffer_size : int
  ; uppercase : bool }

let fmt_of_byte uppercase
  : Format.formatter -> int -> unit
  = fun ppf x -> match uppercase with
  | true -> Format.fprintf ppf "%02X" x
  | false -> Format.fprintf ppf "%02x" x

let code_to_utf_8 ppf code =
  match (Char.chr (code land 0xff)) with
  | '\032' .. '\126' as chr -> Format.pp_print_char ppf chr
  | _ -> Format.pp_print_char ppf '.'

let pp_like_xxd (xxd:xxd) ~seek ppf chunk =
  assert (String.length chunk <= xxd.cols) ;

  Format.fprintf ppf "%08x: " seek ;

  let off = ref 0 in
  let len = String.length chunk in
  while !off < xxd.cols do
    if !off < len && Option.map_value ~default:true (fun long -> seek + !off < long) xxd.long
    then let code = Char.code (String.get chunk !off) in
         (Fmt.styled xxd.colorscheme.(code) (fmt_of_byte xxd.uppercase)) ppf code
    else Format.pp_print_string ppf "  " ;

    if xxd.groupsize <> 0
       && (!off + 1) mod xxd.groupsize = 0
    then Format.pp_print_string ppf " " ;

    incr off
  done ;

  Format.pp_print_string ppf "  " ;

  let off = ref 0 in
  while !off < xxd.cols do
    if !off < len && Option.map_value ~default:true (fun long -> seek + !off < long) xxd.long
    then let code = Char.code (String.get chunk !off) in
         (Fmt.styled xxd.colorscheme.(code) code_to_utf_8) ppf code
    else Format.pp_print_string ppf " " ;
    incr off
  done ;

  Format.fprintf ppf "@,"

type pp = seek:int -> Format.formatter -> payload -> unit

let pp_middle ppf = function
  | `List -> Format.pp_print_string ppf "; \""
  | `Array -> Format.pp_print_string ppf " ; \""

let pp_chunk ppf chunk =
  for i = 0 to String.length chunk - 1
  do match chunk.[i] with
    | '"' | '*' -> Format.pp_print_char ppf '.'
    | '\032' .. '\126' as chr -> Format.pp_print_char ppf chr
    | _ -> Format.pp_print_char ppf '.' done

let pp_like_caml caml ~seek:_ ppf chunk =
  assert (String.length chunk <= caml.cols) ;

  Format.fprintf ppf "%a" pp_middle caml.kind ;

  (* TODO: [long] *)
  for i = 0 to String.length chunk - 1
  do Format.fprintf ppf ("\\x" ^^ (if caml.uppercase then "%02X" else "%02x"))
      (Char.code (String.get chunk i)) done ;

  Format.pp_print_string ppf "\"" ;

  if String.length chunk < caml.cols
  then
    for _ = String.length chunk to caml.cols - 1
    do Format.pp_print_string ppf "    " done ;

  if caml.with_comments
  then ( Format.fprintf ppf " (* %a *)" pp_chunk chunk ) ;

  Format.fprintf ppf "@,"

type cfg =
  | Xxd of xxd
  | Caml of caml

let pp_of_cfg : cfg -> pp = function
  | Xxd xxd -> pp_like_xxd xxd
  | Caml caml -> pp_like_caml caml

let pp_begin_of_cfg : cfg -> Format.formatter -> unit -> unit = function
  | Xxd _ -> fun _ppf () -> ()
  | Caml caml -> match caml.kind with
    | `List -> fun ppf () ->
      Format.pp_print_string ppf "[\n"
    | `Array -> fun ppf () ->
      Format.pp_print_string ppf "[|\n"

let pp_end_of_cfg : cfg -> Format.formatter -> unit -> unit = function
  | Xxd _ -> fun _ppf () -> ()
  | Caml caml -> match caml.kind with
    | `List -> fun ppf () ->
      Format.pp_print_string ppf "]\n"
    | `Array -> fun ppf () ->
      Format.pp_print_string ppf " |"
    ; Format.pp_close_box ppf ()
    ; Format.pp_print_string ppf "]\n"

let default =
  Xxd { cols= 16
      ; groupsize= 2
      ; long= None
      ; uppercase= false
      ; i_buffer_size= 4096
      ; o_buffer_size= 4096
      ; colorscheme= Array.make 256 `None }

let xxd ?(cols= 16) ?(groupsize= 2) ?long
    ?buffer_size:((i_buffer_size, o_buffer_size)= 4096, 4096) ?(uppercase= false) colorscheme =
  Xxd { cols
      ; groupsize
      ; long
      ; uppercase
      ; i_buffer_size
      ; o_buffer_size
      ; colorscheme }

let caml
    ?(with_comments= false) ?(cols= 16) ?long
    ?buffer_size:((i_buffer_size, o_buffer_size)= 4096, 4096) ?(uppercase= false) kind =
  Caml { kind
       ; with_comments
       ; cols
       ; long
       ; i_buffer_size
       ; o_buffer_size
       ; uppercase }

let cols = function
  | Xxd xxd -> xxd.cols
  | Caml caml -> caml.cols

let long = function
  | Xxd xxd -> xxd.long
  | Caml caml -> caml.long

let i_buffer_size = function
  | Xxd xxd -> xxd.i_buffer_size
  | Caml caml -> caml.i_buffer_size

let o_buffer_size = function
  | Xxd xxd -> xxd.o_buffer_size
  | Caml caml -> caml.o_buffer_size

let o :
  type fi fo s e.
  cfg ->
  s scheduler ->
  (fi, bytes, s, e) iflow -> (fo, string, s, e) oflow ->
  fi -> fo ->
  (fi, s, e) seek ->
  [ `Absolute of int | `Relative of int ] ->
  Format.formatter ->
  ((unit, e) result, s) io =
  fun cfg s (module IFlow) (module OFlow) ic oc { lseek } seek ppf ->
  let ( >>= ) = s.bind in
  let return = s.return in

  let i_buffer_size = i_buffer_size cfg in
  let o_buffer_size = o_buffer_size cfg in
  let itmp = Bytes.create i_buffer_size in
  let otmp = Bytes.create o_buffer_size in

  let cur = Bytes.create (cols cfg) in

  let qui, _ = Ke.create ~capacity:(i_buffer_size * 2) Bigarray.Char in
  let quo, _ = Ke.create ~capacity:(o_buffer_size * 2) Bigarray.Char in

  let pp = pp_of_cfg cfg in
  let pp_begin = pp_begin_of_cfg cfg in
  let pp_end = pp_end_of_cfg cfg in

  let got = ref 0 in
  let input flow buf ~off ~len = match long cfg with
    | None -> IFlow.input flow buf ~off ~len
    | Some long ->
      let len = (min : int -> int -> int) (long - !got) len in
      IFlow.input flow buf ~off ~len >>= function
      | Ok len -> got := !got + len ; return (Ok len)
      | Error _ as err -> return err in

  let rec trailing_output seek =
    if Ke.is_empty quo then return (Ok ())
    else
      let len = (min : int -> int -> int) (Ke.length quo) o_buffer_size in
      Ke.keep_exn quo ~blit:B.blit_to_bytes ~length:Bytes.length ~off:0 ~len otmp ;
      OFlow.output oc (Bytes.unsafe_to_string otmp) ~off:0 ~len >>= function
      | Ok wrote -> Ke.shift_exn quo wrote ; trailing_output seek
      | Error _ as err -> return err

  and trailing_input seek =
    if Ke.is_empty qui
    then
      ( let res = Fmt.strf_like ppf "%a" pp_end () in
        pp_end ppf ()
      ; match Ke.push quo ~blit:B.blit_of_string ~length:String.length res with
      | Some _ -> trailing_output seek
      | None ->
        let len = (min : int -> int -> int) (Ke.length quo) o_buffer_size in
        Ke.keep_exn quo ~blit:B.blit_to_bytes ~length:Bytes.length ~off:0 ~len otmp ;
        OFlow.output oc (Bytes.unsafe_to_string otmp) ~off:0 ~len >>= function
        | Ok wrote -> Ke.shift_exn quo wrote ; trailing_output seek
        | Error _ as err -> return err )
    else
      let len = (min : int -> int -> int) (Ke.length qui) (cols cfg) in
      Ke.keep_exn qui ~blit:B.blit_to_bytes ~length:Bytes.length ~off:0 ~len cur ;

      let tmp = Bytes.sub_string cur 0 len in
      let res = Fmt.strf_like ppf "%a" (pp ~seek) tmp in

      pp ~seek ppf tmp ;

      match Ke.push quo ~blit:B.blit_of_string ~length:String.length res with
      | Some _ ->
        Ke.shift_exn qui len ;
        trailing_input (seek + len)
      | None ->
        let len = (min : int -> int -> int) (Ke.length quo) o_buffer_size in
        Ke.keep_exn quo ~blit:B.blit_to_bytes ~length:Bytes.length ~off:0 ~len otmp ;
        OFlow.output oc (Bytes.unsafe_to_string otmp) ~off:0 ~len >>= function
        | Ok wrote -> Ke.shift_exn quo wrote ; trailing_input seek
        | Error _ as err -> return err

  and flush_output seek =
    match Ke.keep qui ~blit:B.blit_to_bytes ~length:Bytes.length ~off:0 ~len:(cols cfg) cur with
    | None -> flush_input seek (* no enough *)
    | Some () ->
      let tmp = Bytes.sub_string cur 0 (cols cfg) in
      let res = Fmt.strf_like ppf "%a" (pp ~seek) tmp in

      pp ~seek ppf tmp ;

      match Ke.push quo ~blit:B.blit_of_string ~length:String.length res with
      | Some _ ->
        Ke.shift_exn qui (cols cfg) ;
        flush_output (seek + (cols cfg))
      | None ->
        let len = (min : int -> int -> int) (Ke.length quo) o_buffer_size in
        Ke.keep_exn quo ~blit:B.blit_to_bytes ~length:Bytes.length ~off:0 ~len otmp ;
        OFlow.output oc (Bytes.unsafe_to_string otmp) ~off:0 ~len >>= function
        | Ok wrote -> Ke.shift_exn quo wrote ; flush_input seek
        | Error _ as err -> return err

  and flush_input seek =
    if Ke.available qui = 0 then flush_output seek
    else
      let len = (min : int -> int -> int) i_buffer_size (Ke.available qui) in
      input ic itmp ~off:0 ~len >>= function
      | Error _ as err -> return err
      | Ok 0 ->
        trailing_input seek
      | Ok len ->
        let _ = Ke.push_exn qui ~blit:B.blit_of_bytes ~length:Bytes.length ~off:0 ~len itmp in
        flush_output seek in

  let res = Fmt.strf_like ppf "%a" pp_begin () in
  let _ = Ke.push_exn quo ~blit:B.blit_of_string ~length:String.length res in
  pp_begin ppf () ;

  match seek with
  | `Absolute position ->
    if position < 0
    then lseek ic position `END >>= function
      | Ok _ -> flush_input 0
      | Error _ as err -> return err
    else flush_input position
  | `Relative position ->
    (lseek ic position `CUR >>= function
      | Ok _ -> flush_input 0
      | Error _ as err -> return err)
