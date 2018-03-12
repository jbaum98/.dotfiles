{ pkgs } :
with builtins;
let
  inherit (pkgs) lib;

  nixFilesIn = dir:
    let children = readDir dir;
        f = path: type:
          let absPath = dir + "/${path}"; in
          if type == "directory" then nixFilesIn absPath
          else if lib.hasSuffix ".nix" (baseNameOf path) then [absPath]
          else [];
    in concatLists (lib.mapAttrsToList f children);

  mergeSets = foldl' (a: b: a // b) {};

  importAll = map (path: pkgs.callPackage path {});

  stowed = mergeSets (importAll (nixFilesIn ~/.nixpkgs/stowed));
in
{
  inherit stowed;
  allowUnfree = true;
  allowBroken = true;
}
