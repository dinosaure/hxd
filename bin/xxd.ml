let ( <.> ) f g x = f (g x)
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

module Result = struct
  include Result

  let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt
end

let x = Array.make 256 `None

let notzen =
  for i = 0 to 31 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xd7, 0xff))
  done
  ; for i = 48 to 57 do
      x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xdf, 0x77))
    done
  ; for i = 65 to 90 do
      x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0x5f))
    done
  ; for i = 97 to 122 do
      x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0xd7))
    done
  ; Hxd.colorscheme_of_array x

let pixzen =
  [|
     0x000000; 0x560000; 0x640000; 0x750000; 0x870000; 0x9b0000; 0xb00000
   ; 0xc60000; 0xdd0000; 0xf50000; 0xff0f0f; 0xff2828; 0xff4343; 0xff5e5e
   ; 0xff7979; 0xfe9595; 0x4c1600; 0x561900; 0x641e00; 0x752300; 0x872800
   ; 0x9b2e00; 0xb03400; 0xc63b00; 0xdd4200; 0xf54900; 0xff570f; 0xff6928
   ; 0xff7b43; 0xff8e5e; 0xffa179; 0xfeb595; 0x4c3900; 0x564000; 0x644b00
   ; 0x755700; 0x876500; 0x9b7400; 0xb08400; 0xc69400; 0xdda600; 0xf5b800
   ; 0xffc30f; 0xffc928; 0xffd043; 0xffd65e; 0xffdd79; 0xfee495; 0x4c4c00
   ; 0x565600; 0x646400; 0x757500; 0x878700; 0x9b9b00; 0xb0b000; 0xc6c600
   ; 0xdddd00; 0xf5f500; 0xffff0f; 0xffff28; 0xffff43; 0xffff5e; 0xffff79
   ; 0xfffe95; 0x324c00; 0x395600; 0x426400; 0x4e7500; 0x5a8700; 0x679b00
   ; 0x75b000; 0x84c600; 0x93dd00; 0xa3f500; 0xafff0f; 0xb7ff28; 0xc0ff43
   ; 0xc9ff5e; 0xd2ff79; 0xdbfe95; 0x1f4c00; 0x235600; 0x296400; 0x307500
   ; 0x388700; 0x409b00; 0x49b000; 0x52c600; 0x5cdd00; 0x66f500; 0x73ff0f
   ; 0x82ff28; 0x91ff43; 0xa1ff5e; 0xb1ff79; 0xc1fe95; 0x004c00; 0x005600
   ; 0x006400; 0x007500; 0x008700; 0x009b00; 0x00b000; 0x00c600; 0x00dd00
   ; 0x00f500; 0x0fff0f; 0x28ff28; 0x43ff43; 0x5eff5e; 0x79ff79; 0x95fe95
   ; 0x004c19; 0x00561c; 0x006421; 0x007527; 0x00872d; 0x009b33; 0x00b03a
   ; 0x00c642; 0x00dd49; 0x00f551; 0x0fff5f; 0x28ff70; 0x43ff81; 0x5eff93
   ; 0x79ffa6; 0x95feb8; 0x004c4c; 0x005656; 0x006464; 0x007575; 0x008787
   ; 0x009b9b; 0x00b0b0; 0x00c6c6; 0x00dddd; 0x00f5f5; 0x0ffffe; 0x28fffe
   ; 0x43fffe; 0x5efffe; 0x79ffff; 0x95fffe; 0x00394c; 0x004056; 0x004b64
   ; 0x005775; 0x006587; 0x00749b; 0x0084b0; 0x0094c6; 0x00a6dd; 0x00b8f5
   ; 0x0fc3ff; 0x28c9ff; 0x43d0ff; 0x5ed6ff; 0x79ddff; 0x95e4fe; 0x00264c
   ; 0x002b56; 0x003264; 0x003a75; 0x004387; 0x004d9b; 0x0058b0; 0x0063c6
   ; 0x006edd; 0x007af5; 0x0f87ff; 0x2893ff; 0x43a1ff; 0x5eaeff; 0x79bcff
   ; 0x95cafe; 0x00134c; 0x001556; 0x001964; 0x001d75; 0x002187; 0x00269b
   ; 0x002cb0; 0x0031c6; 0x0037dd; 0x003df5; 0x0f4bff; 0x285eff; 0x4372ff
   ; 0x5e86ff; 0x799aff; 0x95b0fe; 0x19004c; 0x1c0056; 0x210064; 0x270075
   ; 0x2d0087; 0x33009b; 0x3a00b0; 0x4200c6; 0x4900dd; 0x5100f5; 0x5f0fff
   ; 0x7028ff; 0x8143ff; 0x935eff; 0xa679ff; 0xb895fe; 0x33004c; 0x390056
   ; 0x420064; 0x4e0075; 0x5a0087; 0x67009b; 0x7500b0; 0x8400c6; 0x9300dd
   ; 0xa300f5; 0xaf0fff; 0xb728ff; 0xc043ff; 0xc95eff; 0xd279ff; 0xdb95fe
   ; 0x4c004c; 0x560056; 0x640064; 0x750075; 0x870087; 0x9b009b; 0xb000b0
   ; 0xc600c6; 0xdd00dd; 0xf500f5; 0xfe0fff; 0xfe28ff; 0xfe43ff; 0xfe5eff
   ; 0xfe79ff; 0xfe95fe; 0x4c0032; 0x560039; 0x640042; 0x75004e; 0x87005a
   ; 0x9b0067; 0xb00075; 0xc60084; 0xdd0093; 0xf500a3; 0xff0faf; 0xff28b7
   ; 0xff43c0; 0xff5ec9; 0xff79d2; 0xffffff
  |]

let pixzen =
  Hxd.colorscheme_of_array
    (Array.map
       (fun v ->
         `Style (`Bg, `bit24 (v lsr 16, (v lsr 8) land 0xff, v land 0xff)))
       pixzen)

let null = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let do_cmd () cols groupsize long uppercase pixel seek ic oc =
  let ic, ic_close =
    match ic with
    | `Stdin -> stdin, fun () -> ()
    | `File path ->
      let ic = open_in path in
      ic, fun () -> close_in ic in
  let oc, oc_close =
    match oc with
    | `Stdout -> stdout, fun () -> ()
    | `File path ->
      let oc = open_out path in
      oc, fun () -> close_out oc in
  let colorscheme = if pixel then pixzen else notzen in
  let configuration = Hxd.xxd ?cols ?groupsize ?long ~uppercase colorscheme in
  let res = Hxd_unix.generate configuration ic oc seek null in
  ic_close ()
  ; oc_close ()
  ; Result.map_error (fun (`Msg err) -> Format.asprintf "%s." err) res

let do_fmt style_renderer =
  (* XXX(dinosaure): [fmt] hacks on tags to turn on colors. *)
  let style_renderer =
    match style_renderer with
    | Some `Ansi -> `Ansi
    | Some `None -> `None
    | None ->
      let dumb =
        match Sys.getenv_opt "TERM" with
        | Some "dumb" | Some "" | None -> true
        | _ -> false in
      let isatty =
        try Unix.(isatty (descr_of_out_channel Stdlib.stdout))
        with Unix.Unix_error _ -> false in
      if (not dumb) && isatty then `Ansi else `None in
  Hxd.Fmt.set_style_renderer null style_renderer

let string_for_all p str =
  let cursor = ref 0 in
  let len = String.length str in
  while !cursor < len && p str.[!cursor] do
    incr cursor
  done
  ; !cursor = len

let is_digit = function '0' .. '9' -> true | _ -> false

let parser_seek str =
  let cursor = ref 0 in
  let is_relative =
    match str.[!cursor] with
    | '+' -> incr cursor ; true
    | _ -> false
    | exception _ -> false in
  let from_end =
    match str.[!cursor] with
    | '-' -> incr cursor ; true
    | _ -> false
    | exception _ -> false in
  let number = String.sub str !cursor (String.length str - !cursor) in
  match is_relative, from_end, int_of_string number with
  | true, true, n -> `Relative (-n)
  | true, false, n -> `Relative n
  | false, true, n -> `Absolute (-n)
  | false, false, n -> `Absolute n
  | exception _ -> invalid_arg "Invalid number: %S" number

open Cmdliner

let ic =
  let pp ppf = function
    | `Stdin -> Format.pp_print_string ppf "<stdin>"
    | `File path -> Format.pp_print_string ppf path in
  let parser x =
    match Sys.file_exists x with
    | true -> Result.ok (`File x)
    | false -> Result.error_msgf "%s not found" x in
  Arg.conv ~docv:"<infile>" (parser, pp)

let dir_sep_char = '/'

let validate_and_collapse_seps p =
  let max_idx = String.length p - 1 in
  let rec with_buf b last_sep k i =
    if i > max_idx then Bytes.sub_string b 0 k
    else
      let c = p.[i] in
      if c = '\x00' then invalid_arg "Invalid path %S" p
      ; if c <> dir_sep_char then (
          Bytes.set b k c
          ; with_buf b false (succ k) (succ i))
        else if not last_sep then (
          Bytes.set b k c
          ; with_buf b true (succ k) (succ i))
        else with_buf b true k (succ i) in
  let rec try_no_alloc last_sep i =
    if i > max_idx then p
    else
      let c = p.[i] in
      if c = '\x00' then invalid_arg "Invalid path %S" p
      ; if c <> dir_sep_char then try_no_alloc false (succ i)
        else if not last_sep then try_no_alloc true (succ i)
        else
          let b = Bytes.of_string p in
          with_buf b true i (succ i) in
  let start = if max_idx > 0 then if p.[0] = dir_sep_char then 1 else 0 else 0 in
  try_no_alloc false start

let oc =
  let pp ppf = function
    | `Stdout -> Format.pp_print_string ppf "<stdout>"
    | `File path -> Format.pp_print_string ppf path in
  let parser str =
    match validate_and_collapse_seps str with
    | x -> Result.ok (`File x :> [ `File of string | `Stdout ])
    | exception _ -> Result.error_msgf "Invalid path %S" str in
  Arg.conv ~docv:"<outfile>" (parser, pp)

let cols =
  let parser x =
    match int_of_string x with
    | n ->
      if n < 1 || n > 256 then
        Result.error_msgf "Invalid <cols> value (must <= 256 && > 0): %d" n
      else Result.ok n
    | exception _ -> Result.error_msgf "Invalid <cols> value: %S" x in
  Arg.conv ~docv:"<cols>" (parser, Format.pp_print_int)

let seek =
  let pp ppf = function
    | `Absolute x -> Format.pp_print_int ppf x
    | `Relative x -> Format.fprintf ppf "+%d" x in
  let parser str =
    match parser_seek str with
    | v -> Ok v
    | exception _ -> Result.error_msgf "Invalid seek value: %S" str in
  Arg.conv ~docv:"<seek>" (parser, pp)

let number =
  let parser x =
    match int_of_string x with
    | n ->
      if n < 0 then
        Result.error_msgf "Invalid <number> value (must be positive): %d" n
      else Result.ok n
    | exception _ -> Result.error_msgf "Invalid <number> value: %S" x in
  Arg.conv ~docv:"<number>" (parser, Format.pp_print_int)

let cols =
  let doc = "Format <cols> octets per line. Default 16. Max 256." in
  Arg.(value & opt (some cols) None & info ["c"; "cols"] ~doc ~docv:"<cols>")

let groupsize =
  let doc =
    "Separate the output of every <bytes> bytes (two hex characters) by a \
     whitespace. Specify -g 0 to suppress grouping. <bytes> defaults to 2."
  in
  Arg.(
    value
    & opt (some number) None
    & info ["g"; "groupsize"] ~doc ~docv:"<bytes>")

let long =
  let doc = "Stop after writing <len> octets." in
  Arg.(value & opt (some number) None & info ["l"; "len"] ~doc ~docv:"<len>")

let uppercase =
  let doc = "Use upper case hex letters. Default is lower case." in
  Arg.(value & flag & info ["u"] ~doc)

let pixel =
  let doc = "Use background colors instead of foreground colors." in
  Arg.(value & flag & info ["p"] ~doc)

let ic = Arg.(value & pos 0 ic `Stdin & info [] ~docv:"<infile>")
let oc = Arg.(value & pos 1 oc `Stdout & info [] ~docv:"<outfile>")

let seek =
  let doc =
    "Start at <seek> bytes abs. (or rel.) infile offset. + indicates thatseek \
     is relative to the current stdin file position (meaningless when not \
     reading from stdin). : indicates that the seek should be that many \
     characters from the end of the input (or if combined with +, before the \
     current stdin file position). Without -s option, xxd starts at the \
     current file position." in
  Arg.(value & opt seek (`Relative 0) & info ["s"; "seek"] ~doc ~docv:"<seek>")

(* (c) Daniel Bünzli *)
let style_renderer ?env () =
  let enum = ["auto", None; "always", Some `Ansi; "never", Some `None] in
  let color = Arg.enum enum in
  let enum_alts = Arg.doc_alts_enum enum in
  let doc =
    Format.asprintf "Colorize the output. $(docv) must be %s." enum_alts in
  Arg.(value & opt color None & info ["color"] ?env ~doc ~docv:"<when>")

let setup_fmt =
  let env = Cmd.Env.info "HXD_COLOR" in
  Term.(const do_fmt $ style_renderer ~env ())

let cmd =
  let doc = "Make a hexdump." in
  let man =
    [
      `S "DESCRIPTION"
    ; `P
        "$(tname) creates a hex dump of a given file or standard input. It \
         allows the transmission of binary data in a mail-safe ASCII \
         representation. It can be used to perform binary file patching."
    ] in
  let info = Cmd.info "xxd" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info
    Term.(
      const do_cmd
      $ setup_fmt
      $ cols
      $ groupsize
      $ long
      $ uppercase
      $ pixel
      $ seek
      $ ic
      $ oc)

let () = exit (Cmd.eval_result cmd)
