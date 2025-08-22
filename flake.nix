{
  description = "Grace Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # OCaml overlay
    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    with inputs;
      flake-utils.lib.eachDefaultSystem (system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            ocaml-overlay.overlays.default
            (import ./nix/overlay.nix)
          ];
        };

        grace = pkgs.callPackage ./nix/grace.nix {};

        fmt = treefmt.lib.evalModule pkgs {
          projectRootFile = "flake.nix";

          programs.alejandra.enable = true;
          programs.ocamlformat = {
            enable = true;
            package = pkgs.ocamlformat_0_26_1;
          };

          settings.global.excludes = ["result" "assets" ".direnv" "_build" "_opam" "node_modules"];
        };
      in {
        packages = {
          inherit grace;
          default = grace;
        };

        formatter = fmt.config.build.wrapper;

        devShells.default = pkgs.mkShell {
          name = "grace-dev-shell";

          inputsFrom = [grace];

          buildInputs = with pkgs; [
            # Formatters
            alejandra
            ocamlformat_0_26_1

            # OCaml devenv
            ocamlPackages.utop
            ocamlPackages.ocaml-lsp
            ocamlPackages.merlin
            ocamlPackages.merlin-lib
            ocamlPackages.ocaml
            ocamlPackages.dune
          ];
        };
      });
}
