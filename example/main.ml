#!/usr/bin/env ocaml

;;
#use "topfind"

;;
#require "fmt"

;;
#require "hxd.string"

let text =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ut nibh a \
   erat cursus auctor in quis felis. Sed facilisis ex eu vestibulum eleifend. \
   In nec libero congue, varius enim eu, bibendum ligula. Aenean in ultrices \
   justo, et ultricies elit. Pellentesque mattis leo eu finibus maximus. Nulla \
   auctor sagittis odio, nec bibendum tortor blandit quis. Nulla a risus nec \
   ante dapibus pellentesque. Suspendisse rutrum ornare ante, non finibus erat \
   sollicitudin et. Pellentesque rhoncus faucibus pellentesque. Pellentesque \
   ullamcorper leo eget diam aliquam, in dignissim neque tempus. Nulla iaculis \
   est ac aliquet euismod. Nunc ut nibh eget metus bibendum rutrum ac a magna. \
   Suspendisse fringilla laoreet lacinia. Quisque pellentesque at nunc ut \
   pulvinar. Maecenas vitae diam lacus. Cras tempor vestibulum vehicula. Etiam \
   eu nulla sit amet nulla molestie malesuada ut nec elit. Phasellus sagittis \
   condimentum ultrices. Vestibulum dapibus dictum turpis, ac fringilla quam. \
   Nulla laoreet eleifend eros, at tristique lacus faucibus a. Fusce at turpis \
   at augue tristique pulvinar vel vitae nisi."

let () = Fmt.pr "text: @[<hov>%a@]\n" (Hxd_string.pp Hxd.O.default) text
