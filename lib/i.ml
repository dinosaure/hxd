[@@@warning "-16-32"]

module Option = struct
  let some x = Some x
end

type configuration =
  { cols : int }

let configuration ?(cols= 16) =
  { cols }

let default =
  { cols= 16 }

type chunk =
    Chunk of { seek : int option
             ; payload : string
             ; rest : (unit, (int * string)) result }

let of_hex chr = match chr with
  | '0' .. '9' -> Char.code chr - 48
  | 'a' .. 'f' -> Char.code chr - 87
  | 'A' .. 'F' -> Char.code chr - 55
  | _ -> assert false


let to_number str =
  let len = String.length str - 1 in
  let rec go i acc =
    if i > len then acc
    else go (succ i) (acc * 16 + of_hex str.[i]) in
  go 0 0

module Parser = struct
  open Angstrom
  open Astring

  let is_relaxed_hexa chr =
    Char.Ascii.is_hex_digit chr || Char.Ascii.is_blank chr

  let seek =
    take_while is_relaxed_hexa >>= fun relaxed_hexa ->
    char ':' *> return (String.filter Char.Ascii.is_hex_digit relaxed_hexa)
    >>| to_number

  let of_2 a b = of_hex a * 16 + of_hex b
  let of_1 a = of_hex a

  let byte =
    let hex = satisfy Char.Ascii.is_hex_digit in
    (lift2 of_2 hex hex) <|> (lift of_1 hex)

  let at_most n p =
    let rec go acc = function
      | 0 -> return (List.rev acc)
      | n -> (p >>| fun x -> `Continue x) <|> (return `Stop) >>= function
        | `Continue x -> go (x :: acc) (pred n)
        | `Stop -> return (List.rev acc) in
    go [] n

  let payload cols =
    let blank = skip_while Char.Ascii.is_blank in
    at_most cols (blank *> byte <* blank) >>| fun l ->
    let res = Bytes.create (List.length l) in
    List.iteri (fun i x -> Bytes.set res i (Char.of_byte x)) l ;
    Bytes.unsafe_to_string res

  let is_newline = function '\n' -> true | _ -> false

  let compare a pp =
    match pp with
    | '\032' .. '\126' -> ((=) : char -> char -> bool) a pp
    | _ -> ((=) : char -> char -> bool) '.' pp

  let rest payload =
    take_till is_newline >>=
    (fun pprint ->
       let exception Malformed in
       let len = String.length pprint in
       let pos = ref 0 in
       try String.iteri (fun i x -> if i >= len || compare x pprint.[i] then ( pos := i ; raise Malformed ) ) payload
         ; return (Ok ())
       with Malformed -> return (Error (!pos, pprint)))
    <* char '\n'

  let chunk cols =
    option None (seek >>| Option.some) >>= fun seek ->
    payload cols >>= fun payload ->
    rest payload >>= fun rest ->
    return (Chunk { seek; payload; rest; })
end

let chunk_of_string configuration x =
  match Angstrom.parse_string (Parser.chunk configuration.cols) x with
  | Ok chunk -> chunk
  | Error _ -> Fmt.invalid_arg "Qxxd.chunk_of_string: universal parser does not accept %S" x

let has_newline buf =
  let exception Found in
  let len = Bigstringaf.length buf in
  let pos = ref 0 in
  try
      for i = 0 to len - 1
      do if Bigstringaf.unsafe_get buf i = '\n'
         then ( pos := i ; raise Found ) done
    ; None
  with Found -> Some !pos
