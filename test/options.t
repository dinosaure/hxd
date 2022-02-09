Tests about options
  $ echo -n "foo & bar" | hxd.xxd -g 4
  00000000: 666f6f20 26206261 72                 foo & bar
  $ echo -n "foo & bar" | hxd.xxd -g 8
  00000000: 666f6f2026206261 72                foo & bar
  $ echo -n "foo & bar" | hxd.xxd -g 9
  00000000: 666f6f202620626172                 foo & bar
  $ echo -n "foo & bar" | hxd.xxd -c 9
  00000000: 666f 6f20 2620 6261 72  foo & bar
  $ echo -n "foo & bar" | hxd.xxd -c 1
  00000000: 66  f
  00000001: 6f  o
  00000002: 6f  o
  00000003: 20   
  00000004: 26  &
  00000005: 20   
  00000006: 62  b
  00000007: 61  a
  00000008: 72  r
  $ echo -n "abababababababab" | hxd.xxd -g 3
  00000000: 616261 626162 616261 626162 616261 62  abababababababab
  $ echo -n "ababab" | hxd.xxd -c 3
  00000000: 6162 61  aba
  00000003: 6261 62  bab
  $ echo -n "Zjklmno" | hxd.xxd
  00000000: 5a6a 6b6c 6d6e 6f                        Zjklmno
  $ echo -n "Zjklmno" | hxd.xxd -u
  00000000: 5A6A 6B6C 6D6E 6F                        Zjklmno
  $ echo -n "" | hxd.xxd -s +1
  xxd: sorry cannot seek.
  [123]
  $ echo -n "foo & bar" | hxd.xxd -s +6
  00000006: 6261 72                                  bar
  $ echo -n "foo & bar" | hxd.xxd -s 6
  00000006: 6261 72                                  bar
  $ echo "foo & bar" | hxd.xxd -s 6 -l 3
  00000006: 6261 72                                  bar
  $ echo -n "foo & bar" | hxd.xxd -l 3
  00000000: 666f 6f                                  foo
  $ echo -n "abababababababab" | hxd.xxd -g 16
  00000000: 61626162616261626162616261626162  abababababababab
  $ echo -n "abababababababab" | hxd.xxd -g 3
  00000000: 616261 626162 616261 626162 616261 62  abababababababab
  $ echo -n "0000000000000000abababab" | hxd.xxd -c 2 -g 3
  00000000: 3030  00
  00000002: 3030  00
  00000004: 3030  00
  00000006: 3030  00
  00000008: 3030  00
  0000000a: 3030  00
  0000000c: 3030  00
  0000000e: 3030  00
  00000010: 6162  ab
  00000012: 6162  ab
  00000014: 6162  ab
  00000016: 6162  ab
