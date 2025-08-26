{
  lib,
  ocamlPackages,
}:
with ocamlPackages;
  buildDunePackage {
    pname = "grace";
    version = "dev";

    src = lib.cleanSource ../.;

    propagatedBuildInputs = [core ppx_jane fmt dedent iter core_unix uutf ppx_optcomp ppx_compare ppx_sexp_conv ppx_hash];
  }
