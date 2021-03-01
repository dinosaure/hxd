(** HeXDump library for OCaml.

    The library provides the most abstract way to serialize
    something to a human-readable {i hexdump}. It allows the
    transmission of binary data in a `mail-safe' ASCII
    representation and it can handle colors if the given
    [Format.formatter] supports it.

    It permits to serialize to a {i caml} value as a simple
    list of [string]s or an array of [string]s. *)

module Fmt : sig
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

  val set_style_renderer : Format.formatter -> [ `Ansi | `None ] -> unit
  (** [set_style_renderer ppf mode] sets the style renderer of [ppf] to [mode]. *)
end

type colorscheme
(** The type of colorschemes. *)

val colorscheme_of_array : Fmt.style array -> colorscheme
(** [colorscheme_of_array arr] returns a {!colorscheme} from an array of 256 elements.
    Otherwise, it returns an invalid argument. *)

val lowercase : colorscheme -> Fmt.style -> unit
(** [lowercase c style] sets lowercase ASCII values to the style [style]. *)

val uppercase : colorscheme -> Fmt.style -> unit
(** [uppercase c style] sets uppercase ASCII values to the style [style]. *)

val digit : colorscheme -> Fmt.style -> unit
(** [digit c style] sets digit ASCII values to the style [style]. *)

val code : colorscheme -> int -> Fmt.style -> unit
(** [code c code style] sets a specific ASCII code [c] to the style [style]. *)

type cfg
(** The type of configurations. *)

val xxd :
     ?cols:int
  -> ?groupsize:int
  -> ?long:int
  -> ?uppercase:bool
  -> colorscheme
  -> cfg
(** [xxd ?cols ?groupsize ?long ?uppercase colorscheme] returns a configuration
   which can be used by {!generate} then.
    - [cols]: octets per line (default to 16)
    - [groupsize]: Separate the output of every [groupsize] bytes
      by a whitespace (default to 2).
    - [long]: stop after reading [len] octets.
    - [uppercase]: use upper case hex letters (default is lower case).
    - [colorscheme]: {!colorscheme} used if the given {!Format.formatter}
      supports it. *)

val caml :
     ?with_comments:bool
  -> ?cols:int
  -> ?long:int
  -> ?uppercase:bool
  -> [ `List | `Array ]
  -> cfg
(** [caml ?with_comments ?cols ?long ?uppercase k] returns a configuration
   which can be used by {!generate} then. It allows to produces a {i caml}
   value from something.
    - [cols]: octets per line (default to 16)
    - [with_comments]: add a comment which pretty-line the group
      in a comment
    - [long]: stop after reading [len] octets.
    - [k]: if the user wants to produce a list of [string]s or
      an array of [string]s. *)

val default : cfg
(** A default [XXD] configuration. *)

type ('a, 's) io

type ('f, 'b, 's, 'e) input =
  'f -> 'b -> off:int -> len:int -> ((int, 'e) result, 's) io

type ('f, 'b, 's, 'e) output =
  'f -> 'b -> off:int -> len:int -> ((int, 'e) result, 's) io

module Make (S : sig
  type 'a t
end) : sig
  type t
  type 'a s = 'a S.t

  external inj : 'a s -> ('a, t) io = "%identity"
  external prj : ('a, t) io -> 'a s = "%identity"
end

type 's scheduler = {
    bind: 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io
  ; return: 'a. 'a -> ('a, 's) io
}

type ('f, 's, 'e) seek = {
    lseek: 'f -> int -> [ `SET | `CUR | `END ] -> ((int, 'e) result, 's) io
}

val generate :
     cfg
  -> 's scheduler
  -> ('i, bytes, 's, 'e) input
  -> ('o, string, 's, 'e) output
  -> 'i
  -> 'o
  -> ('i, 's, 'e) seek
  -> [ `Absolute of int | `Relative of int ]
  -> Format.formatter
  -> ((unit, 'e) result, 's) io
(** [generate cfg scheduler input output ic oc seek pos ppf] is the most
   abstract way to produce an {i hex-dump} output. According to arguments,
   we are able to read into [ic] and write into [oc] with respectively
   [input] and [output].

    [seek] is used to manipulate the position in [ic] according to the
   given position [pos].

    [ppf] is used to know if we support colors or not. [generate] writes on
   it too and it takes care about {i pretty-printing boxes}.

    [scheduler] depends on which scheduler you use. You need to create one
   over {i monads}:

    {[
      module Unix_scheduler = Hxd.Make(struct type 'a t = 'a end)

      let unix_scheduler =
        let open Unix_scheduler in
        { Hxd.bind= (fun x f -> f (prj x))
        ; return= inj }

      generate cfg unix_scheduler ...
    ]}

    You can abstract LWT {i monads} too:

    {[
      module Lwt_scheduler = Hxd.Make(struct type 'a t = 'a Lwt.t end)

      let lwt_scheduler =
        let open Lwt.Infix in
        let open Lwt_scheduler in
        { Hxd.bind= (fun f x -> inj (prj x >>= fun x -> prj (f x)))
        ; return= (fun x -> inj (Lwt.return x)) }

      generate cfg lwt_scheduler
    ]}

    Such layers exist with [hxd.unix] and [hxd.lwt].
*)
