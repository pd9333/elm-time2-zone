{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  name = "elm-time2-zone";
  buildInputs = with pkgs; [ just nodejs python2 ];
}
