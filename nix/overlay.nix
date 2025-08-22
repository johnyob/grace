final: prev:
with prev; {
  ocamlPackages = final.ocaml-ng.ocamlPackages_5_3;

  ocaml-ng =
    ocaml-ng
    // (with ocaml-ng; {
      ocamlPackages_5_3 = ocamlPackages_5_3.overrideScope (
        _: prev:
          with prev; {
          }
      );
    });
}
