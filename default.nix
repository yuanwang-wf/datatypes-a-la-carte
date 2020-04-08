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
in
myHaskellPackages.callPackage ./datatypes-a-la-carte.nix {}
