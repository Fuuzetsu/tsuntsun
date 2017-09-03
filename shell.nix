{ ghc, pkgs ? import <nixpkgs> {} }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "tesseract-env";
  buildInputs = with pkgs;
    [ cairo
      gtk3
      pango
      pkgconfig
      scrot
      tesseract
      xlibs.libX11
      xlibs.xproto
      zlib
    ];
}
