type ('a, 'b) t =
  { mutable r: int
  ; mutable w: int
  ; c: int
  ; k: ('a, 'b) Bigarray.kind
  ; v: ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t }

exception Empty
exception Full

let[@inline always] to_power_of_two v =
  let res = ref (pred v) in
  res := !res lor (!res lsr 1) ;
  res := !res lor (!res lsr 2) ;
  res := !res lor (!res lsr 4) ;
  res := !res lor (!res lsr 8) ;
  res := !res lor (!res lsr 16) ;
  succ !res

let[@inline always] mask t v = v land (t.c - 1)
let[@inline always] empty t = t.r = t.w
let[@inline always] size t = t.w - t.r
let[@inline always] full t = size t = t.c
let[@inline always] available t = t.c - (t.w - t.r)
let is_empty t = (empty [@inlined]) t
let length q = size q

let create ?capacity kind =
  let capacity =
    match capacity with
    | None | Some 0 -> 1
    | Some n ->
      if n < 0 then Fmt.invalid_arg "Ke.create"
      else to_power_of_two n
  in
  ( { r= 0
    ; w= 0
    ; c= capacity
    ; k= kind
    ; v= Bigarray.Array1.create kind Bigarray.c_layout capacity }
  , capacity )

type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit
type 'a length = 'a -> int

let push_exn t ~blit ~length ?(off = 0) ?len v =
  let len = match len with None -> length v - off | Some len -> len in
  if (available [@inlined]) t < len then raise Full ;
  let msk = (mask [@inlined]) t t.w in
  let pre = t.c - msk in
  let rst = len - pre in
  let ret =
    if rst > 0 then (
      blit v off t.v msk pre ;
      blit v (off + pre) t.v 0 rst ;
      [ Bigarray.Array1.sub t.v ((mask [@inlined]) t t.w) pre
      ; Bigarray.Array1.sub t.v 0 rst ] )
    else (
      blit v off t.v msk len ;
      [Bigarray.Array1.sub t.v ((mask [@inlined]) t t.w) len] )
  in
  t.w <- t.w + len ;
  ret

let push t ~blit ~length ?off ?len v =
  try Some (push_exn t ~blit ~length ?off ?len v) with Full -> None

let keep_exn t ~blit ~length ?(off = 0) ?len v =
  let len = match len with None -> length v - off | Some len -> len in
  if (size [@inlined]) t < len then raise Empty ;
  let msk = (mask [@inlined]) t t.r in
  let pre = t.c - msk in
  let rst = len - pre in
  if rst > 0 then (
    blit t.v msk v off pre ;
    blit t.v 0 v (off + pre) rst )
  else blit t.v msk v off len

let keep t ~blit ~length ?off ?len v =
  try Some (keep_exn t ~blit ~length ?off ?len v) with Empty -> None

let peek t =
  let len = (size [@inlined]) t in
  if len == 0 then []
  else
    let msk = (mask [@inlined]) t t.r in
    let pre = t.c - msk in
    let rst = len - pre in
    if rst > 0 then
      [ Bigarray.Array1.sub t.v msk pre
      ; Bigarray.Array1.sub t.v 0 rst ]
    else [ Bigarray.Array1.sub t.v msk len ]

let unsafe_shift t len = t.r <- t.r + len

let shift_exn t len =
  if (size [@inlined]) t < len then raise Empty ;
  unsafe_shift t len
