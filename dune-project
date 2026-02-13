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
 (version 0.3.0)
 (synopsis
  "A fancy diagnostics library that allows your compilers to exit with grace")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  (fmt
   (>= 0.8.7))
  iter
  uutf
  (dedent :with-test)
  (core :with-test)
  (ppx_jane :with-test)
  ))

