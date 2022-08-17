{ lib, buildGoModule }:

let
  src = (import ./sources.nix).ghz;
in
buildGoModule {
  inherit src;
  vendorSha256 = "qZD+qxjjFgyQDtjOQcilS4w2sS9I+7iCK2/ThaAJTy4=";
  pname = src.repo;
  version = src.branch;
  meta = with lib; {
    inherit (src) homepage description;
    license = licenses.asl20;
    platforms = platforms.all;
  };

  doCheck = false;
}
