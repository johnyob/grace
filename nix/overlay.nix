final: prev:
with prev; {
  ocamlPackages = final.ocaml-ng.ocamlPackages_5_5;

  ocaml-ng =
    ocaml-ng
    // (with ocaml-ng; {
      ocamlPackages_5_5 = ocamlPackages_5_5.overrideScope (
        _: prev:
          with prev; {
          }
      );
    });
}
