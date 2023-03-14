(lang dune 3.4)

(name grace)

(generate_opam_files true)

(license MIT)

(maintainers "alistair.obrien@trili.tech")

(authors "Alistair O'Brien")

(package
 (name grace)
 (synopsis
  "A fancy diagnostics library that allows your compilers to exit with grace")
 (depends dune core ppx_jane fmt))

(package
 (name fmt_doc)
 (depends dune core ppx_jane fmt))
