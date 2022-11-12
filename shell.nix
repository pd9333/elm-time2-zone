{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  name = "elm-time2-zone";
  buildInputs = with pkgs; [
    elmPackages.elm
    elmPackages.elm-format
    jq
    just
    nixfmt
    nodejs
    poetry
    python2
    python3
  ];
}
