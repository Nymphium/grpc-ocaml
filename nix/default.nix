{ pkgs, lib, fetchFromGitHub, ocamlPackages }:
let
  opam2nix = pkgs.callPackage ./opam2nix.nix {
    ocamlPackagesOverride = ocamlPackages;
  };

  localPackages =
    let contents = builtins.attrNames (builtins.readDir ../.);
    in builtins.filter (lib.strings.hasSuffix ".opam") contents;

  src =
    let sources = import ./sources.nix {};
        src' = pkgs.nix-gitignore.gitignoreSource [ "*.nix" ] ../.;
    in
    { inherit (sources) ocaml-protoc-plugin; }
    // (builtins.listToAttrs
        (builtins.map (name': { name = lib.strings.removeSuffix ".opam" name'; value = src'; }) localPackages) );

  devPackages = [ "utop" "ocaml-lsp-server" "odoc" ];

  # package names with {with-test} in opam;
  # opam2nix does not support the flag at present.
  testPackages = [ "conf-protoc" "ppx_inline_test" "alcotest" ];

  args = {
    ocaml = ocamlPackages.ocaml;
    selection = ./opam-selection.nix;
    inherit src;
  };

  opam = opam2nix.build (args // {
    override = { pkgs, ... }: {
      easy-format = super: super.overrideAttrs (_: {
        buildPhase = ''
          dune build -p easy-format @install
        '';
      });
    };
  });

  resolve = opam2nix.resolve args (devPackages ++ localPackages ++ testPackages ++ [
    "${src.ocaml-protoc-plugin}/ocaml-protoc-plugin.opam"
    # (toString ./. + "/opam/core_unix.opam")
  ]);
in
{
  inherit opam resolve;
  devInputs = builtins.map (pkg: builtins.getAttr pkg opam) (devPackages ++ testPackages);
}
