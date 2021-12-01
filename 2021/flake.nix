{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.unison.url = "github:ceedubs/unison-nix";

  outputs = { nixpkgs, flake-utils, unison, ... }: flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (
    system:
    let
      pkgs = import nixpkgs { inherit system; overlays = [ unison.overlay ]; };
    in
    {
      devShell = pkgs.mkShell { buildInputs = [ pkgs.unison-ucm pkgs.scala-cli ]; };
    }
  );
}
