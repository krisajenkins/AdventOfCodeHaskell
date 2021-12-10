{ sources ? import ./nix/sources.nix }:
let
  pkgs =
    import sources.nixpkgs {
      overlays = [ ];
    };
in
pkgs.mkShell
{
  buildInputs = [
    pkgs.stack
    pkgs.cabal-install
    pkgs.haskell-language-server
    pkgs.libiconv
  ];
}
