{
  lib,
  ocamlPackages,
}:
with ocamlPackages;
  buildDunePackage {
    pname = "grace";
    version = "dev";

    src = lib.cleanSource ../.;

    propagatedBuildInputs = [core ppx_jane fmt dedent iter core_unix uutf ppx_optcomp];
  }
