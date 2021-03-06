Tests about caml outputs
  $ echo -n "foo" | hxd.caml
  [ "\x66\x6f\x6f" ]
  $ echo -n "Hello World!" | hxd.caml > main.ml
  $ ocamlopt main.ml
  $ ./a.out
  $ echo -n "abababababababab" | hxd.caml
  [ "\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62" ]
  $ printf "abababababababab\0" | hxd.caml
  [ "\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62"
  ; "\x00" ]
  $ echo -n "aaaaaaaaa" | hxd.caml
  [ "\x61\x61\x61\x61\x61\x61\x61\x61\x61" ]
  $ echo -n "" | hxd.caml
  $ printf "\0" | hxd.caml
  [ "\x00" ]
  $ echo -n "Hello World!" > input
  $ hxd.caml input
  [ "\x48\x65\x6c\x6c\x6f\x20\x57\x6f\x72\x6c\x64\x21" ]
  $ hxd.caml zh.txt
  [ "\xe8\xaf\xb6\x20\xe6\xaf\x94\x20\xe8\xa5\xbf\x20\xe8\xbf\xaa\x20"
  ; "\xe4\xbc\x8a\x20\xe8\x89\xbe\xe5\xbc\x97\x20\xe5\x90\x89\x20\xe8"
  ; "\x89\xbe\xe5\xb0\xba\x20\xe8\x89\xbe\x20\xe6\x9d\xb0\x20\xe5\xbc"
  ; "\x80\x20\xe8\x89\xbe\xe5\x8b\x92\x20\xe8\x89\xbe\xe9\xa9\xac\x20"
  ; "\xe8\x89\xbe\xe5\xa8\x9c\x0a\xe4\xb8\x80\x20\xe4\xba\x8c\x20\x20"
  ; "\xe4\xb8\x89\x20\xe5\x9b\x9b\x20\x20\xe4\xba\x94\x20\xe5\x85\xad"
  ; "\x20\xe4\xb8\x83\x20\xe4\xb9\x9d\x20\xe5\x8d\x81\x0a" ]
  $ cat >main.ml <<EOF
  > let input =
  > EOF
  $ hxd.caml zh.txt >> main.ml
  $ cat >>main.ml <<EOF
  > let () = List.iter print_string input
  > EOF
  $ ocamlopt main.ml
  $ ./a.out > result.out
  $ diff zh.txt result.out
  $ echo -n "abababababababab" | hxd.caml -c2
  [ "\x61\x62"
  ; "\x61\x62"
  ; "\x61\x62"
  ; "\x61\x62"
  ; "\x61\x62"
  ; "\x61\x62"
  ; "\x61\x62"
  ; "\x61\x62" ]
  $ echo -n "foo & bar" | hxd.caml -k array
  [| "\x66\x6f\x6f\x20\x26\x20\x62\x61\x72" |]
  $ cat >main.ml <<EOF
  > let input =
  > EOF
  $ echo "foo & bar" | hxd.caml -k array >> main.ml
  $ cat >>main.ml <<EOF
  > let () = Array.iter print_string input
  > EOF
  $ ocamlopt main.ml
  $ ./a.out
  foo & bar
  $ yes "Hello World!" 2> /dev/null | head -n 10 | hxd.caml --kind=array
  [| "\x48\x65\x6c\x6c\x6f\x20\x57\x6f\x72\x6c\x64\x21\x0a\x48\x65\x6c"
   ; "\x6c\x6f\x20\x57\x6f\x72\x6c\x64\x21\x0a\x48\x65\x6c\x6c\x6f\x20"
   ; "\x57\x6f\x72\x6c\x64\x21\x0a\x48\x65\x6c\x6c\x6f\x20\x57\x6f\x72"
   ; "\x6c\x64\x21\x0a\x48\x65\x6c\x6c\x6f\x20\x57\x6f\x72\x6c\x64\x21"
   ; "\x0a\x48\x65\x6c\x6c\x6f\x20\x57\x6f\x72\x6c\x64\x21\x0a\x48\x65"
   ; "\x6c\x6c\x6f\x20\x57\x6f\x72\x6c\x64\x21\x0a\x48\x65\x6c\x6c\x6f"
   ; "\x20\x57\x6f\x72\x6c\x64\x21\x0a\x48\x65\x6c\x6c\x6f\x20\x57\x6f"
   ; "\x72\x6c\x64\x21\x0a\x48\x65\x6c\x6c\x6f\x20\x57\x6f\x72\x6c\x64"
   ; "\x21\x0a" |]
  $ yes "Hello World!" 2> /dev/null | head -n100 > file
  $ sh -c "dd of=plain_snippet status=none bs=1k count=1; hxd.caml -s +128 > caml_snippet" < file
  $ cat >main.ml <<EOF
  > let input = 
  > EOF
  $ cat caml_snippet >> main.ml
  $ cat >>main.ml <<EOF
  > let () = List.iter print_string input
  > EOF
  $ ocamlopt main.ml
  $ ./a.out
  rld!
  Hello World!
  Hello World!
  Hello World!
  Hello World!
  Hello World!
  Hello World!
  Hello World!
  Hello World!
  Hello World!
  Hello World!
  Hello World!
  $ echo "foo & bar" | hxd.caml --with-comments
  [ "\x66\x6f\x6f\x20\x26\x20\x62\x61\x72\x0a" ]                         (* foo & bar.       *)
  $ echo "foo & bar" | hxd.caml --with-comments -c 10
  [ "\x66\x6f\x6f\x20\x26\x20\x62\x61\x72\x0a" ] (* foo & bar. *)
  $ echo -n "abababababababab" | hxd.caml --with-comments
  [ "\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62" ] (* abababababababab *)
  $ printf "abababababababab\0" | hxd.caml --with-comments
  [ "\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62"   (* abababababababab *)
  ; "\x00" ]                                                             (* .                *)
  $ cat >main.ml <<EOF
  > let input =
  > EOF
  $ echo "Hello World!" | hxd.caml --with-comments >> main.ml
  $ cat >>main.ml <<EOF
  > let () = List.iter print_string input
  > EOF
  $ ocamlopt main.ml
  $ ./a.out
  Hello World!
  $ echo "abababababababab" | hxd.caml --with-comments -k array
  [| "\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62"    (* abababababababab *)
   ; "\x0a" |]                                                             (* .                *)
  $ echo -n "abababababababababababababababab" | hxd.caml --with-comments -k array
  [| "\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62"    (* abababababababab *)
   ; "\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62\x61\x62" |] (* abababababababab *)
  $ echo "foo & bar" | hxd.caml --with-comments -k array
  [| "\x66\x6f\x6f\x20\x26\x20\x62\x61\x72\x0a" |]                         (* foo & bar.       *)
  $ echo "foo & bar" | hxd.caml --with-comments -k array -c10
  [| "\x66\x6f\x6f\x20\x26\x20\x62\x61\x72\x0a" |] (* foo & bar. *)
  $ echo "Hello World!"
  Hello World!
