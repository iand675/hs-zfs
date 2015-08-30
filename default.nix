{ mkDerivation, base, bytestring, c2hs, stdenv, text, zfs }:
mkDerivation {
  pname = "zfs";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base bytestring text ];
  buildTools = [ c2hs ];
  extraLibraries = [ zfs ];
  homepage = "http://github.com/iand675/hs-zfs";
  description = "libzfs bindings";
  license = stdenv.lib.licenses.bsd3;
}
