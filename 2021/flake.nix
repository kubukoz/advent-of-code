{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.unison.url = "github:ceedubs/unison-nix";
  inputs.unison.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { nixpkgs, flake-utils, unison, ... }: flake-utils.lib.eachSystem [ "x86_64-darwin" "aarch64-darwin" ] (
    system:
    let
      x86_pkgs = import nixpkgs { localSystem = "x86_64-darwin"; overlays = [ unison.overlay ]; };

      aarch64-overrides = final: prev:
        if prev.stdenv.isAarch64 then {
          inherit (x86_pkgs) unison-ucm;
        } else { };


      pkgs = import nixpkgs { inherit system; overlays = [ unison.overlay aarch64-overrides ]; };

    in
    {
      devShell = pkgs.mkShell { buildInputs = [ pkgs.unison-ucm (pkgs.sbt.override { jre = pkgs.openjdk11; }) ]; };
    }
  );
}
