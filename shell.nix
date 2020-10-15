let
  pkgs = import <nixpkgs> { };
in
pkgs.runCommand "language-scala-shell" {
  buildInputs = [
    pkgs.binutils
    pkgs.jq
    pkgs.nodejs
    pkgs.ghcid
    pkgs.cabal-install
    (pkgs.haskellPackages.ghcWithHoogle (p: [
      p.prettyprinter_1_7_0
      p.aeson
      p.tasty
      p.tasty-golden
    ]))
  ];
} ""
