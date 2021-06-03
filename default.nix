{ mkDerivation, stdenv, ghc, base, pure-elm, pure-txt
}:
mkDerivation {
  pname = "pure-backdrop";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-elm pure-txt ];
  homepage = "github.com/grumply/pure-backdrop";
  license = stdenv.lib.licenses.bsd3;
}
