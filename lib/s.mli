type ('a, 's) io

type 's scheduler =
  { bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io
  ; return : 'a. 'a -> ('a, 's) io }

type ('f, 's, 'e) seek =
  { lseek : 'f -> int -> [ `SET | `CUR | `END ] -> ((int, 'e) result, 's) io }

module type X = sig
  type 'a s
  type t

  external inj : 'a s -> ('a, t) io = "%identity"
  external prj : ('a, t) io -> 'a s = "%identity"
end

module type FUNCTOR = sig type 'a t end
module Make (T : FUNCTOR) : X with type 'a s = 'a T.t

module type IFLOW = sig
  type scheduler
  type buffer
  type error
  type flow

  val input : flow -> buffer -> off:int -> len:int -> ((int, error) result, scheduler) io
end

module type OFLOW = sig
  type scheduler
  type buffer
  type error
  type flow

  val output : flow -> buffer -> off:int -> len:int -> ((int, error) result, scheduler) io
end

type ('f, 'b, 's, 'e) iflow =
  (module IFLOW with type flow = 'f and type buffer = 'b and type scheduler = 's and type error = 'e)
type ('f, 'b, 's, 'e) oflow =
  (module OFLOW with type flow = 'f and type buffer = 'b and type scheduler = 's and type error = 'e)

