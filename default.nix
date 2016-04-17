{ stdenv, haskellPackages }:

let
  hsEnv = haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [ bytestring-mmap HFuse hashable unordered-containers ]);
in
stdenv.mkDerivation {
  name = "narparser";
  src = ./.;

  buildInputs = [ hsEnv ];
}
