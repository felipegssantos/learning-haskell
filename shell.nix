{ pkgs ? import <nixpkgs> {} }:

with pkgs; mkShell {
  buildInputs = [
    ghc
    haskell-language-server
    cabal-install
  ];
}

