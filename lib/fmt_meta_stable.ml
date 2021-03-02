type Format.stag += Hxd_style_renderer

let style_renderer_tag = Hxd_style_renderer
let meta_store ppf = Format.pp_get_formatter_stag_functions ppf ()
let set_meta_store ppf store = Format.pp_set_formatter_stag_functions ppf store
let meta_raw store tag = store.Format.mark_open_stag tag

let set_meta ppf store ~style_renderer =
  let meta = function
    | Hxd_style_renderer -> style_renderer
    | _ -> "Hxd: god, we broken everythings" in
  let store = {store with Format.mark_open_stag= meta} in
  set_meta_store ppf store
