(lang dune 3.4)

(name grace)

(generate_opam_files true)

(license MIT)

(maintainers "alistair.obrien@trili.tech")

(authors "Alistair O'Brien")

(source
 (github johnyob/grace))

(package
 (name grace)
 (version 0.1.0)
 (synopsis
  "A fancy diagnostics library that allows your compilers to exit with grace")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  core
  ppx_jane
  (fmt
   (>= 0.8.7))
  dedent
  iter
  core_unix
  uutf
  ppx_optcomp))
