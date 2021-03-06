Simple tests
  $ echo -n "foo" | hxd.xxd
  00000000: 666f 6f                                  foo
  $ echo -n "aaaaaaaa" | hxd.xxd
  00000000: 6161 6161 6161 6161                      aaaaaaaa
  $ echo -n "abababab" | hxd.xxd
  00000000: 6162 6162 6162 6162                      abababab
  $ echo -n "abababababababab" | hxd.xxd
  00000000: 6162 6162 6162 6162 6162 6162 6162 6162  abababababababab
  $ echo -n "" | hxd.xxd
  $ printf '\0' | hxd.xxd
  00000000: 00                                       .
  $ printf 'abababababababab\0' | hxd.xxd
  00000000: 6162 6162 6162 6162 6162 6162 6162 6162  abababababababab
  00000010: 00                                       .
  $ yes "Hello World!" 2> /dev/null | head -n 10 | hxd.xxd
  00000000: 4865 6c6c 6f20 576f 726c 6421 0a48 656c  Hello World!.Hel
  00000010: 6c6f 2057 6f72 6c64 210a 4865 6c6c 6f20  lo World!.Hello 
  00000020: 576f 726c 6421 0a48 656c 6c6f 2057 6f72  World!.Hello Wor
  00000030: 6c64 210a 4865 6c6c 6f20 576f 726c 6421  ld!.Hello World!
  00000040: 0a48 656c 6c6f 2057 6f72 6c64 210a 4865  .Hello World!.He
  00000050: 6c6c 6f20 576f 726c 6421 0a48 656c 6c6f  llo World!.Hello
  00000060: 2057 6f72 6c64 210a 4865 6c6c 6f20 576f   World!.Hello Wo
  00000070: 726c 6421 0a48 656c 6c6f 2057 6f72 6c64  rld!.Hello World
  00000080: 210a                                     !.
  $ echo -n "Hello World!" > input
  $ hxd.xxd input
  00000000: 4865 6c6c 6f20 576f 726c 6421            Hello World!
  $ hxd.xxd zh.txt
  00000000: e8af b620 e6af 9420 e8a5 bf20 e8bf aa20  ... ... ... ... 
  00000010: e4bc 8a20 e889 bee5 bc97 20e5 9089 20e8  ... ...... ... .
  00000020: 89be e5b0 ba20 e889 be20 e69d b020 e5bc  ..... ... ... ..
  00000030: 8020 e889 bee5 8b92 20e8 89be e9a9 ac20  . ...... ...... 
  00000040: e889 bee5 a89c 0ae4 b880 20e4 ba8c 2020  .......... ...  
  00000050: e4b8 8920 e59b 9b20 20e4 ba94 20e5 85ad  ... ...  ... ...
  00000060: 20e4 b883 20e4 b99d 20e5 8d81 0a          ... ... ....
  $ echo "Hello World!" > file
  $ sh -c "cat > plain_copy ; hxd.xxd -s 0 > hex_copy" < file
  $ cat plain_copy
  Hello World!
  $ cat hex_copy
  00000000: 4865 6c6c 6f20 576f 726c 6421 0a         Hello World!.
  $ yes "Hello World!" 2> /dev/null | head -n100 > file
  $ sh -c "dd of=plain_snippet status=none bs=1k count=1; hxd.xxd -s +128 > hex_snippet" < file
  $ hxd.xxd -s 0x480 file > result.out
  $ diff hex_snippet result.out
  $ sh -c "dd of=plain_snippet status=none bs=1k count=1; hxd.xxd -s +-768 > hex_snippet" < file
  $ hxd.xxd -s 0x100 file > result.out
  $ diff hex_snippet result.out
