Tests with caml
  $ cat >main.ml <<EOF
  > let default = Hxd.default
  > 
  > let () =
  >   Format.printf "%a%!" (Hxd_string.pp default) "Hello World!"
  > EOF
  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries hxd.string))
  > EOF
  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF
  $ dune build --display=short ./main.exe
        ocamlc .main.eobjs/byte/dune__exe__Main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/dune__exe__Main.{cmx,o}
      ocamlopt main.exe
  $ dune exec ./main.exe
  00000000: 4865 6c6c 6f20 576f 726c 6421            Hello World!
  $ cat >main.ml <<EOF
  > let default = Hxd.default
  > 
  > let () =
  >   Format.printf "raw: @[<hov>%a@]"
  >     (Hxd_string.pp default) (String.make 32 'x')
  > EOF
  $ dune build --display=short ./main.exe
        ocamlc .main.eobjs/byte/dune__exe__Main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/dune__exe__Main.{cmx,o}
      ocamlopt main.exe
  $ dune exec ./main.exe
  raw: 00000000: 7878 7878 7878 7878 7878 7878 7878 7878  xxxxxxxxxxxxxxxx
       00000010: 7878 7878 7878 7878 7878 7878 7878 7878  xxxxxxxxxxxxxxxx
