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
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskell-language-server
    pkgs.libiconv
    (pkgs.writeScriptBin "testw" "stack test --file-watch --ta '--match Year2021'")
  ];
}
