open Crowbar
open Hxd

let () =
  add_test ~name:"universal-parser" [ bytes ] @@ fun bytes ->
  let bytes = bytes ^ "\n" in
  try let _ = I.chunk_of_string I.default bytes in ()
  with _ -> Crowbar.fail "chunk_of_string fails"
