v0.4.0 2026-02-18 Paris (France)
--------------------------------

- Add a new kind when we output with `hxd.caml`: the string kind (@reynir, @dinosaure, #23)
- Still escape few more characters when we print comments (@dinosaure, db68d47)

v0.3.6 2025-11-29 Paris (France)
--------------------------------

* Fix the `caml` support (@dinosaure, #21)
  + Escape the character '"'
  + Print out into `ppf` when we configure hxd with `caml`

v0.3.5 2025-10-01 Paris (France)
--------------------------------

* Fix the compilation with `cmdliner.2.0.0` (@dinosaure, #19)

v0.3.4 2025-06-02 Paris (France)
--------------------------------

* Add `x-maintenance-intent` into the OPAM file (@hannesm, #16)
* Be compatible with OCaml 5.04 (alpha1) (@dinosaure, #17)

v0.3.3 2024-12-11 Paris (France)
--------------------------------

* Fix the usage of formatters to be composable with `fmt` (@dinosaure, #14)
* Update the codebase with `ocamlformat.0.27.0` (@dinosaure, #14)

v0.3.2 2022-02-28 Paris (France)
--------------------------------

* Prevent tests using yes from failing if SIGPIPE is trapped (@sternenseemann, #11)
* Upgrade `hxd` with `cmdliner.1.1.0` (@MisterDA, #12)

v0.3.1 2021-04-03 Paris (France)
--------------------------------

- Fix the array printer (#9, @dinosaure)

v0.3.0 2021-02-03 Paris (France)
--------------------------------

- Add LICENSE.md
- Keep compatibility with 4.06
- Big improvement on the produced tool
- Lint dependencies

v0.2.0 2020-05-05 Paris (France)
--------------------------------

- Update to use `angstrom.0.14.0`
- Use `stdlib-shims` and remove deprecated functions about formatter

v0.1.0 2019-07-04 Paris (France)
--------------------------------

- First release
