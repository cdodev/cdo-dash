{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "14e7d85431e9f9838d7107d18cb79c7fa534f54e";
      sha256 = "0lmkppidmhnayv0919990ifdd61f9d23dzjzr8amz7hjgc74yxs0";
    }
  ) {
    inherit pkgs;
  };

in
pkgs.mkShell {
  buildInputs = [ easy-ps.purs easy-ps.spago easy-ps.spago2nix pkgs.cacert pkgs.yarn pkgs.nodejs-10_x ];
}
