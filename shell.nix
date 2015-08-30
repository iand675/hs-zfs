with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, c2hs, stdenv, system-filepath
             , text, zfs
             }:
             mkDerivation {
               pname = "zfs";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base bytestring system-filepath text ];
               buildTools = [ c2hs ];
               extraLibraries = [ zfs ];
               homepage = "http://github.com/iand675/hs-zfs";
               description = "libzfs bindings";
               license = stdenv.lib.licenses.bsd3;
               preConfigure = "echo hi";
                 # "sed -i -e /extra-lib-dirs/d -e 's|, /usr/include, /usr/local/include/mesos||' hs-mesos.cabal";
             }) {};
in
  pkg.env
