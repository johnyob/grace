{
  lib,
  ocamlPackages,
}:
with ocamlPackages;
  buildDunePackage {
    pname = "grace";
    version = "dev";

    src = lib.cleanSource ../.;

    propagatedBuildInputs = [fmt dedent iter uutf];
    checkInputs = [core ppx_jane dedent];
  }
