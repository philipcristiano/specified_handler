let
  sysPkg = import <nixpkgs> { };
  releasedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "22.05";
    sha256 = "sha256-M6bJShji9AIDZ7Kh7CPwPBPb/T7RiVev2PAcOi4fxDQ=";
  };
  pkgs = import releasedPkgs {};
  stdenv = pkgs.stdenv;
  extraInputs = sysPkg.lib.optionals stdenv.isDarwin (with sysPkg.darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices]);

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ pkgs.gnumake
                  pkgs.erlangR24
                  pkgs.wget
                ] ++ extraInputs;

}
