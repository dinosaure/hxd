(* (c) Daniel Bünzli *)

open Fmt_meta

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let style_renderer_of_raw = function
  | "\x00" -> `None
  | "\x01" -> `Ansi
  | _ -> `None

let style_renderer_to_raw = function `None -> "\x00" | `Ansi -> "\x01"

let style_renderer ppf =
  let res = meta_raw (meta_store ppf) style_renderer_tag in
  style_renderer_of_raw res

let set_style_renderer ppf renderer =
  if ppf == Format.str_formatter then
    invalid_arg "Impossible to apply style on string formatter"

  ; let store = meta_store ppf in
    let style_renderer = style_renderer_to_raw renderer in

    set_meta ppf store ~style_renderer

let ansi_style_reset = "\x1b[m"

type standard =
  [ `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan | `White ]

type bright = [ `Bright of standard ]
type bit8 = [ `bit8 of int * int * int ]
type bit24 = [ `bit24 of int * int * int ]
type grayscale = [ `Grayscale of int ]

type style =
  [ `None
  | `Style of [ `Fg | `Bg ] * [ standard | bright | bit8 | bit24 | grayscale ]
  ]

type rest = [ standard | bright | bit8 | grayscale ]

let ( .![]<- ) = Bytes.unsafe_set

let to_decdigit buffer off v =
  if v >= 100 then (
    buffer.![off + 2] <- Char.unsafe_chr (48 + (v mod 10))
    ; buffer.![off + 1] <- Char.unsafe_chr (48 + (v / 10 mod 10))
    ; buffer.![off + 0] <- Char.unsafe_chr (48 + (v / 100))
    ; off + 3)
  else if v >= 10 then (
    buffer.![off + 1] <- Char.unsafe_chr (48 + (v mod 10))
    ; buffer.![off + 0] <- Char.unsafe_chr (48 + (v / 10))
    ; off + 2)
  else (
    buffer.![off] <- Char.unsafe_chr (48 + v)
    ; succ off)
  [@@inline]

let ansi_style_code buffer off = function
  | `None ->
    buffer.![off] <- '\x1b'
    ; buffer.![off + 1] <- '['
    ; buffer.![off + 2] <- 'm'
    ; off + 3
  | `Style (style, (#bit24 as color)) ->
    (* let anchor = off in *)
    let (`bit24 (r, g, b)) = color in
    buffer.![off + 0] <- '\x1b'
    ; buffer.![off + 1] <- '['
    ; if style = `Bg then buffer.![off + 2] <- '4' else buffer.![off + 2] <- '3'
    ; buffer.![off + 3] <- '8'
    ; buffer.![off + 4] <- ';'
    ; buffer.![off + 5] <- '2'
    ; buffer.![off + 6] <- ';'
    ; let off = off + 7 in
      let off = to_decdigit buffer off r in
      buffer.![off] <- ';'
      ; let off = to_decdigit buffer (succ off) g in
        buffer.![off] <- ';'
        ; let off = to_decdigit buffer (succ off) b in
          buffer.![off] <- 'm'
          ; (* Format.eprintf ">> %S.\n%!" (Bytes.sub_string buffer anchor ((off + 1) - anchor)) ; *)
            off + 1
  | `Style (style, (#rest as color)) ->
    let color =
      match color with
      | `Black -> 0
      | `Red -> 1
      | `Green -> 2
      | `Yellow -> 3
      | `Blue -> 4
      | `Magenta -> 5
      | `Cyan -> 6
      | `White -> 7
      | `Bright color -> (
        match color with
        | `Black -> 8
        | `Red -> 9
        | `Green -> 10
        | `Yellow -> 11
        | `Blue -> 12
        | `Magenta -> 13
        | `Cyan -> 14
        | `White -> 15)
      | `bit8 (r, g, b) ->
        if r >= 0 && r <= 5 && g >= 0 && g <= 5 && b >= 0 && b <= 5 then
          16 + (36 * r) + (6 * g) + b
        else invalid_arg "Invalid color: bit8(%d, %d, %d)" r g b
      | `Grayscale n ->
        if n >= 0 && n <= 24 then 232 + n
        else invalid_arg "Invalid color: Grayscale(%d)" n in
    buffer.![off + 0] <- '\x1b'
    ; buffer.![off + 1] <- '['
    ; if style = `Bg then buffer.![off + 2] <- '3' else buffer.![off + 2] <- '4'
    ; buffer.![off + 3] <- '8'
    ; buffer.![off + 4] <- ';'
    ; buffer.![off + 5] <- '5'
    ; buffer.![off + 6] <- ';'
    ; let off = to_decdigit buffer (off + 7) color in
      (* TODO *)
      buffer.![off] <- 'm' ; succ off

let styled style pp ppf =
  match style_renderer ppf with
  | `None -> Format.fprintf ppf "%a" pp
  | `Ansi ->
    let reset ppf = Format.fprintf ppf "@<0>%s" ansi_style_reset in
    let res = Bytes.create 19 in
    let len = ansi_style_code res 0 style in
    let str = Bytes.sub_string res 0 len in
    Format.kfprintf reset ppf "@<0>%s%a" str pp

let with_buffer ?like buf =
  let ppf = Format.formatter_of_buffer buf in
  match like with
  | None -> ppf
  | Some like ->
    set_meta_store ppf (meta_store like)
    ; ppf

let strf_like ppf fmt =
  let buf = Buffer.create 80 in
  let bppf = with_buffer ~like:ppf buf in
  let flush ppf =
    Format.pp_print_flush ppf ()
    ; let s = Buffer.contents buf in
      Buffer.reset buf ; s in
  Format.kfprintf flush bppf fmt
