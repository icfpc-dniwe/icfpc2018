{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  lib = pkgs.haskell.lib;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: with lib; {
    };
  };

  drv = lib.doBenchmark (haskellPackages_.callPackage ./default.nix {});

in

  if pkgs.lib.inNixShell then drv.env else drv
