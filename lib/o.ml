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

type configuration =
  { cols : int
  ; groupsize : int
  ; long : int option
  ; uppercase : bool
  ; colorscheme : colorscheme }

let default =
  { cols= 16
  ; groupsize= 2
  ; long= None
  ; uppercase= false
  ; colorscheme= Array.make 256 `None }

let configuration ?(cols= 16) ?(groupsize= 2) ?long ?(uppercase= false) colorscheme =
  { cols
  ; groupsize
  ; long
  ; uppercase
  ; colorscheme }

let fmt_of_configuration configuration
  : Format.formatter -> int -> unit
  = fun ppf x -> match configuration.uppercase with
  | true -> Format.fprintf ppf "%02X" x
  | false -> Format.fprintf ppf "%02x" x

let code_to_utf_8 ppf code =
  match (Char.chr (code land 0xff)) with
  | '\032' .. '\126' as chr -> Format.pp_print_char ppf chr
  | _ -> Format.pp_print_char ppf '.'

let pp_chunk configuration seek ppf chunk =
  assert (String.length chunk <= configuration.cols) ;

  Format.fprintf ppf "%08x: " seek ;

  let off = ref 0 in
  let len = String.length chunk in
  while !off < configuration.cols do
    if !off < len && Option.map_value ~default:true (fun long -> seek + !off < long) configuration.long
    then let code = Char.code (String.get chunk !off) in
         (Fmt.styled configuration.colorscheme.(code) (fmt_of_configuration configuration)) ppf code
    else Format.pp_print_string ppf "  " ;

    if configuration.groupsize <> 0
       && (!off + 1) mod configuration.groupsize = 0
    then Format.pp_print_string ppf " " ;

    incr off
  done ;

  Format.pp_print_string ppf "  " ;

  let off = ref 0 in
  while !off < configuration.cols do
    if !off < len && Option.map_value ~default:true (fun long -> seek + !off < long) configuration.long
    then let code = Char.code (String.get chunk !off) in
         (Fmt.styled configuration.colorscheme.(code) code_to_utf_8) ppf code
    else Format.pp_print_string ppf " " ;
    incr off
  done ;

  Format.pp_print_newline ppf ()

let o :
  type fi fo s e.
  configuration ->
  s scheduler ->
  (fi, bytes, s, e) iflow -> (fo, string, s, e) oflow ->
  fi -> fo ->
  (fi, s, e) seek ->
  [ `Absolute of int | `Relative of int ] ->
  Format.formatter ->
  ((unit, e) result, s) io =
  fun configuration s (module IFlow) (module OFlow) ic oc { lseek } seek ppf ->
  let ( >>= ) = s.bind in
  let return = s.return in

  let i_buffer_size = 4096 in
  let o_buffer_size = 4096 in
  let itmp = Bytes.create i_buffer_size in
  let otmp = Bytes.create o_buffer_size in

  let cur = Bytes.create configuration.cols in

  let qui, _ = Ke.create ~capacity:(i_buffer_size * 2) Bigarray.Char in
  let quo, _ = Ke.create ~capacity:(o_buffer_size * 2) Bigarray.Char in

  let got = ref 0 in
  let input flow buf ~off ~len = match configuration.long with
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
    if Ke.is_empty qui then trailing_output seek
    else
      let len = (min : int -> int -> int) (Ke.length qui) configuration.cols in
      Ke.keep_exn qui ~blit:B.blit_to_bytes ~length:Bytes.length ~off:0 ~len cur ;

      let tmp = Bytes.sub_string cur 0 len in
      let res = Fmt.strf_like ppf "%a" (pp_chunk configuration seek) tmp in

      (pp_chunk configuration seek) ppf tmp ;

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
    match Ke.keep qui ~blit:B.blit_to_bytes ~length:Bytes.length ~off:0 ~len:configuration.cols cur with
    | None -> flush_input seek (* no enough *)
    | Some () ->
      let tmp = Bytes.sub_string cur 0 configuration.cols in
      let res = Fmt.strf_like ppf "%a" (pp_chunk configuration seek) tmp in

      (pp_chunk configuration seek) ppf tmp ;

      match Ke.push quo ~blit:B.blit_of_string ~length:String.length res with
      | Some _ ->
        Ke.shift_exn qui configuration.cols ;
        flush_output (seek + configuration.cols)
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
