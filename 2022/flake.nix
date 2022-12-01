{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { flake-utils, self, nixpkgs, ... }: flake-utils.lib.simpleFlake {
    name = "aoc-2022";
    inherit self nixpkgs;
    shell = { pkgs }: pkgs.mkShell {
      packages = [
        # iex
        pkgs.elixir
      ];
    };
  };
}
