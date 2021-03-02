type ('a, 's) io

type 's scheduler = {
    bind: 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io
  ; return: 'a. 'a -> ('a, 's) io
}

type ('f, 's, 'e) seek = {
    lseek: 'f -> int -> [ `SET | `CUR | `END ] -> ((int, 'e) result, 's) io
}

module type X = sig
  type 'a s
  type t

  external inj : 'a s -> ('a, t) io = "%identity"
  external prj : ('a, t) io -> 'a s = "%identity"
end

module Common = struct
  type t

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

module type FUNCTOR = sig
  type 'a t
end

module Make (T : FUNCTOR) = struct
  type 'a s = 'a T.t

  include Common
end

type ('f, 'b, 's, 'e) input =
  'f -> 'b -> off:int -> len:int -> ((int, 'e) result, 's) io

type ('f, 'b, 's, 'e) output =
  'f -> 'b -> off:int -> len:int -> ((int, 'e) result, 's) io
