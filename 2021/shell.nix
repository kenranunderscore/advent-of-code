{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs =
    [ pkgs.ghc pkgs.cabal-install pkgs.haskellPackages.ghcid ];
}
