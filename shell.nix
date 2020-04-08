{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:
let
  bootstrap = import <nixpkgs> {};

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src {};
  myHaskellPackages = pkgs.haskell.packages."${compiler}";

  myPackages = myHaskellPackages.callCabal2nix "project" ./datatypes-a-la-carte.cabal {};
in
myHaskellPackages.shellFor {
  withHoogle = true;
  packages = p: [ myPackages ];
  inherit ((import ./pre-commit.nix).pre-commit-check) shellHook;
  buildInputs = with myHaskellPackages;
    [
      hlint
      ghcid
      cabal2nix
      ormolu
      cabal-install
      cabal-fmt
    ];
}
