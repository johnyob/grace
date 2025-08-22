{
  description = "Grace Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    # Linting
    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-hooks.url = "github:cachix/git-hooks.nix";

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
          programs.ocamlformat.enable = true;

          settings.global.excludes = ["result" "assets" ".direnv" "_build" "_opam" "node_modules"];
        };
      in {
        packages = {
          inherit grace;
          default = grace;
        };

        checks = {
          grace = grace.overrideAttrs (old: {
            name = "check-${old.name}";
            doCheck = true;
          });

          pre-commit-check = git-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              commitizen.enable = true;

              # Configure treefmt to use the above treefmt config
              treefmt.enable = true;
              treefmt.package = fmt.config.build.wrapper;
            };
          };
        };

        formatter = fmt.config.build.wrapper;

        devShells.default = pkgs.mkShell {
          name = "grace-dev-shell";

          inputsFrom = [grace];

          buildInputs = with pkgs; [
            # Formatters
            alejandra
            ocamlformat
            commitizen

            # OCaml devenv
            ocamlPackages.utop
            ocamlPackages.ocaml-lsp
            ocamlPackages.merlin
            ocamlPackages.merlin-lib
            ocamlPackages.ocaml
            ocamlPackages.dune
          ];
        };

        apps.ci-cz-check = {
          type = "app";
          program = let
            czCheck = pkgs.writeShellScript "cz-check.sh" ''
              set -e
              ${pkgs.commitizen}/bin/cz check --rev-range origin/main..
            '';
          in
            builtins.toString czCheck;
        };
      });
}
