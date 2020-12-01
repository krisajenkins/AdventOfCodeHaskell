{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.cabal-install
    pkgs.libiconv
  ];
}
