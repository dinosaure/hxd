(* (c) Daniel Bünzli *)

let invalid_arg fmt =
  Format.kasprintf invalid_arg fmt

let style_renderer_tag = "hxd.style_renderer"

let style_renderer_of_raw = function
  | "\x00" -> `None
  | "\x01" -> `Ansi
  | _ -> `None

let style_renderer_to_raw = function
  | `None -> "\x00"
  | `Ansi -> "\x01"

let meta_store ppf = Format.pp_get_formatter_tag_functions ppf ()
let set_meta_store ppf store = Format.pp_set_formatter_tag_functions ppf store
let meta_raw store tag = store.Format.mark_open_tag tag
let set_meta ppf store ~style_renderer =
  let meta = function
    | "hxd.style_renderer" -> style_renderer
    | _ -> "Hxd: god, we broken everythings" in
  let store = { store with Format.mark_open_tag= meta } in
  set_meta_store ppf store

let style_renderer ppf =
  let res = meta_raw (meta_store ppf) style_renderer_tag in
  style_renderer_of_raw res

let set_style_renderer ppf renderer =
  if ppf == Format.str_formatter
  then invalid_arg "Impossible to apply style on string formatter" ;

  let store = meta_store ppf in
  let style_renderer = style_renderer_to_raw renderer in

  set_meta ppf store ~style_renderer

let ansi_style_reset = "\x1b[m"

type standard =
  [ `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White ]

type bright = [ `Bright of standard ]
type bit8 = [ `bit8 of (int * int * int) ]
type bit24 = [ `bit24 of (int * int * int) ]
type grayscale = [ `Grayscale of int ]

type style =
  [ `None
  | `Style of ([ `Fg | `Bg ] * [ standard
                               | bright
                               | bit8
                               | bit24
                               | grayscale ]) ]

type rest =
  [ standard
  | bright
  | bit8
  | grayscale ]

let ansi_style_code = function
  | `None -> ansi_style_reset
  | `Style (where, (#bit24 as color)) ->
    let `bit24 (r, g, b) = color in
    if r >= 0 && r <= 255
       && g >= 0 && g <= 255
       && b >= 0 && b <= 255
    then let where = match where with `Fg -> 38 | `Bg -> 48 in
      Format.asprintf "\x1b[%d;2;%d;%d;%dm" where r g b
    else invalid_arg "Invalid color: bit24(%d, %d, %d)" r g b
  | `Style (where, (#rest as color))->
    let where = match where with
      | `Fg -> 38 | `Bg -> 48 in
    let color = match color with
      | `Black -> 0 | `Red -> 1 | `Green -> 2 | `Yellow -> 3 | `Blue -> 4 | `Magenta -> 5 | `Cyan -> 6 | `White -> 7
      | `Bright color ->
        (match color with
         | `Black -> 8 | `Red -> 9 | `Green -> 10 | `Yellow -> 11 | `Blue -> 12 | `Magenta -> 13 | `Cyan -> 14 | `White -> 15)
      | `bit8 (r, g, b) ->
        if r >= 0 && r <= 5
           && g >= 0 && g <= 5
           && b >= 0 && b <= 5
        then 16 + 36 * r + 6 * g + b
        else invalid_arg "Invalid color: bit8(%d, %d, %d)" r g b
      | `Grayscale n ->
        if n >= 0 && n <= 24
        then 232 + n
        else invalid_arg "Invalid color: Grayscale(%d)" n in
    Format.asprintf "\x1b[%d;5;%dm" where color

let styled style pp ppf = match style_renderer ppf with
  | `None -> Format.fprintf ppf "%a" pp
  | `Ansi ->
    let reset ppf = Format.fprintf ppf "@<0>%s" ansi_style_reset in
    Format.kfprintf reset ppf "@<0>%s%a" (ansi_style_code style) pp

let with_buffer ?like buf =
  let ppf = Format.formatter_of_buffer buf in
  match like with
  | None -> ppf
  | Some like -> set_meta_store ppf (meta_store like) ; ppf

let strf_like ppf fmt =
  let buf = Buffer.create 80 in
  let bppf = with_buffer ~like:ppf buf in
  let flush ppf =
    Format.pp_print_flush ppf () ;
    let s = Buffer.contents buf in
    Buffer.reset buf ; s in
  Format.kfprintf flush bppf fmt
