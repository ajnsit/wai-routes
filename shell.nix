{nixpkgs ? import <nixpkgs> {}, compiler ? "default"}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  f = import ./.;
  drv = haskellPackages.callPackage f {};

in if pkgs.lib.inNixShell then drv.env else drv
