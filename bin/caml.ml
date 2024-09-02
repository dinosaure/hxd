let ( <.> ) f g x = f (g x)
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

module Result = struct
  include Result

  let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt
end

let null =
  Format.formatter_of_out_functions
    {
      Format.out_string= (fun _ _ _ -> ())
    ; out_flush= (fun () -> ())
    ; out_newline= (fun () -> ())
    ; out_spaces= (fun _ -> ())
    ; out_indent= (fun _ -> ())
    }

let do_cmd cols long uppercase kind with_comments seek ic oc =
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
  let configuration = Hxd.caml ?cols ?long ~uppercase ~with_comments kind in
  let res = Hxd_unix.generate configuration ic oc seek null in
  ic_close ()
  ; oc_close ()
  ; Result.map_error (fun (`Msg err) -> err) res

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
  if not (string_for_all is_digit number) then
    invalid_arg "Invalid number: %S" number
  ; match is_relative, from_end, int_of_string number with
    | true, true, n -> `Relative (-n)
    | true, false, n -> `Relative n
    | false, true, n -> `Absolute (-n)
    | false, false, n -> `Absolute n

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

let kind =
  let pp ppf = function
    | `List -> Format.pp_print_string ppf "list"
    | `Array -> Format.pp_print_string ppf "array" in
  let parser = function
    | "list" -> Ok `List
    | "array" -> Ok `Array
    | x -> Result.error_msgf "Invalid <kind> value: %S" x in
  Arg.conv ~docv:"<kind>" (parser, pp)

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

let with_comments =
  let doc = "Print comments which is a human-readable equivalent of line" in
  Arg.(value & flag & info ["with-comments"] ~doc)

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

let kind =
  let doc =
    "Type of output, if you want a string list (default) or a string array."
  in
  Arg.(
    value
    & opt kind (`List : [ `List | `Array ])
    & info ["k"; "kind"] ~doc ~docv:"<kind>")

let cmd =
  let doc = "Make a hexdump." in
  let man =
    [
      `S "DESCRIPTION"
    ; `P
        "$(tname) creates a camlized hex dump of a given file or standard \
         input. It allows the transmission of binary data in a mail-safe ASCII \
         representation. It can be used to perform binary file patching."
    ] in
  let info = Cmd.info "xdd" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info
    Term.(
      const do_cmd
      $ cols
      $ long
      $ uppercase
      $ kind
      $ with_comments
      $ seek
      $ ic
      $ oc)

let () = exit (Cmd.eval_result cmd)
