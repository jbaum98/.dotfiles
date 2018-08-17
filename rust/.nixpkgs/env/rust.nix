{ rustc, cargo, rustracer, rustfmt }:
let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  myrust = nixpkgs.stable.rust.override {
    extensions = [ "rust-src" ];
  };
in
{
  env.rust = [
    myrust
    (rustracer.overrideAttrs(oldAttrs: rec {
      RUST_SRC_PATH = "${myrust}/lib/rustlib/src/rust/src";
    }))
  ];
}
