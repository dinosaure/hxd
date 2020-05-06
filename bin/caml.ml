let ( <.> ) f g x = f (g x)

module Option = struct
  let map f x = match x with Some x -> Some (f x) | None -> None

  let iter f x = match x with Some x -> f x | None -> ()
end

let x = Array.make 256 `None

let notzen =
  for i = 0 to 31 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xd7, 0xff))
  done ;
  for i = 48 to 57 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xdf, 0x77))
  done ;
  for i = 65 to 90 do
    x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0x5f))
  done ;
  for i = 97 to 122 do
    x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0xd7))
  done ;
  Hxd.O.colorscheme_of_array x

let null =
  Format.formatter_of_out_functions
    {
      Format.out_string = (fun _ _ _ -> ());
      out_flush = (fun () -> ());
      out_newline = (fun () -> ());
      out_spaces = (fun _ -> ());
      out_indent = (fun _ -> ());
    }

let do_cmd cols long uppercase kind with_comments seek ic oc =
  let ic, ic_close =
    match ic with
    | `Stdin -> (stdin, fun () -> ())
    | `File path ->
        let ic = open_in (Fpath.to_string path) in
        (ic, fun () -> close_in ic) in
  let oc, oc_close =
    match oc with
    | `Stdout -> (stdout, fun () -> ())
    | `File path ->
        let oc = open_out (Fpath.to_string path) in
        (oc, fun () -> close_out oc) in
  let configuration = Hxd.O.caml ?cols ?long ~uppercase ~with_comments kind in
  let res = Hxd_unix.o configuration ic oc seek null in
  ic_close () ;
  oc_close () ;
  match res with Ok () -> `Ok () | Error err -> `Error err

let parser_seek x =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let parser =
    let open Angstrom in
    option `None (char '+' *> return `Plus) >>= fun is_relative ->
    option `Positive (char ':' *> return `Negative) >>= fun sign ->
    take_while is_digit >>| fun number ->
    (is_relative, sign, int_of_string number) in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser x with
  | Ok (`None, `Positive, n) -> Ok (`Absolute n)
  | Ok (`None, `Negative, n) -> Ok (`Absolute (-n))
  | Ok (`Plus, `Positive, n) -> Ok (`Relative n)
  | Ok (`Plus, `Negative, n) -> Ok (`Relative (-n))
  | Error _ -> Rresult.R.error_msgf "Invalid <seek> value: %S" x

open Cmdliner

let ic =
  let pp ppf = function
    | `Stdin -> Fmt.string ppf "<stdin>"
    | `File path -> Fpath.pp ppf path in
  let parser x =
    match Sys.file_exists x with
    | true -> Rresult.R.map (fun x -> `File x) (Fpath.of_string x)
    | false -> Rresult.R.error_msgf "%s not found" x in
  Arg.conv ~docv:"<infile>" (parser, pp)

let oc =
  let pp ppf = function
    | `Stdout -> Fmt.string ppf "<stdout>"
    | `File path -> Fpath.pp ppf path in
  let parser = Rresult.R.map (fun x -> `File x) <.> Fpath.of_string in
  Arg.conv ~docv:"<outfile>" (parser, pp)

let cols =
  let pp = Fmt.int in
  let parser x =
    match int_of_string x with
    | n ->
        if n < 1 || n > 256
        then
          Rresult.R.error_msgf "Invalid <cols> value (must <= 256 && > 0): %d" n
        else Rresult.R.ok n
    | exception _ -> Rresult.R.error_msgf "Invalid <cols> value: %S" x in
  Arg.conv ~docv:"<cols>" (parser, pp)

let seek =
  let pp ppf = function
    | `Absolute x -> Fmt.int ppf x
    | `Relative x -> Fmt.pf ppf "+%d" x in
  let parser = parser_seek in
  Arg.conv ~docv:"<seek>" (parser, pp)

let number =
  let pp = Fmt.int in
  let parser x =
    match int_of_string x with
    | n ->
        if n < 0
        then
          Rresult.R.error_msgf "Invalid <number> value (must be positive): %d" n
        else Rresult.R.ok n
    | exception _ -> Rresult.R.error_msgf "Invalid <number> value: %S" x in
  Arg.conv ~docv:"<number>" (parser, pp)

let kind =
  let pp ppf = function
    | `List -> Fmt.string ppf "list"
    | `Array -> Fmt.string ppf "array" in
  let parser = function
    | "list" -> Ok `List
    | "array" -> Ok `Array
    | x -> Rresult.R.error_msgf "Invalid <kind> value: %S" x in
  Arg.conv ~docv:"<kind>" (parser, pp)

let cols =
  let doc = "Format <cols> octets per line. Default 16. Max 256." in
  Arg.(value & opt (some cols) None & info [ "c"; "cols" ] ~doc ~docv:"<cols>")

let groupsize =
  let doc =
    "Separate the output of every <bytes> bytes (two hex characters) by a \
     whitespace. Specify -g 0 to suppress grouping. <bytes> defaults to 2."
  in
  Arg.(
    value
    & opt (some number) None
    & info [ "g"; "groupsize" ] ~doc ~docv:"<bytes>")

let long =
  let doc = "Stop after writing <len> octets." in
  Arg.(value & opt (some number) None & info [ "l"; "len" ] ~doc ~docv:"<len>")

let uppercase =
  let doc = "Use upper case hex letters. Default is lower case." in
  Arg.(value & flag & info [ "u" ] ~doc)

let with_comments =
  let doc = "Print comments which is a human-readable equivalent of line" in
  Arg.(value & flag & info [ "with-comments" ] ~doc)

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
  Arg.(
    value & opt seek (`Relative 0) & info [ "s"; "seek" ] ~doc ~docv:"<seek>")

let kind =
  let doc =
    "Type of output, if you want a string list (default) or a string array."
  in
  Arg.(
    value
    & opt kind (`List : [ `List | `Array ])
    & info [ "k"; "kind" ] ~doc ~docv:"<kind>")

let cmd =
  let doc = "Make a hexdump." in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "$(tname) creates a camlized hex dump of a given file or standard \
         input. It allows the transmission of binary data in a mail-safe ASCII \
         representation. It can be used to perform binary file patching.";
    ] in
  ( Term.(
      pure do_cmd
      $ cols
      $ long
      $ uppercase
      $ kind
      $ with_comments
      $ seek
      $ ic
      $ oc),
    Term.info "xxd" ~version:"%%VERSION%%" ~doc ~man )

let () = Term.(exit @@ eval cmd)
