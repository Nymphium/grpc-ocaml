let
  pkgs = import ./nix/pkgs.nix;
  ocamlPackages = pkgs.callPackage ./nix/ocamlPackages.nix {};
  inherit (pkgs.callPackage ./. { inherit ocamlPackages; }) opam devInputs;

  niv = import ./nix/niv.nix;
  ocamlformat = pkgs.callPackage ./nix/ocamlformat.nix { ocamlformat = ./.ocamlformat; };

  yarn = pkgs.yarn.override {
    nodejs = pkgs.nodejs-slim-16_x;
  };

in
opam.grpc.overrideAttrs (_: {
  buildInputs = devInputs ++ [
    ocamlformat

    # for client test
    yarn
    # for server test
    pkgs.grpcurl
  ];
})
