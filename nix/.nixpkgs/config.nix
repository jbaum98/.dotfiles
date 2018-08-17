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

  importAll = map (path: pkgs.callPackage path {});

  recursiveMergeUpdateUntil = pred: lhs: rhs:
    let f = attrPath:
      lib.zipAttrsWith (n: values:
        let first  = head values;
            rest   = tail values;
            second = head rest;
        in if rest == [] then first
        else if isList first && isList second then second ++ first
        else if pred attrPath second first then first
        else f (attrPath ++ [n]) values
      );
    in f [] [rhs lhs];

  recursiveMergeUpdate = lhs: rhs:
    recursiveMergeUpdateUntil (path: lhs: rhs:
      !(isAttrs lhs && isAttrs rhs)
    ) lhs rhs;

  envLists = (lib.foldl' recursiveMergeUpdate {} (importAll (nixFilesIn ~/.nixpkgs/env))).env;

  toEnv = envName: paths: pkgs.buildEnv {
    name = "${envName}-env";
    inherit paths;
  };

  env = lib.mapAttrs toEnv envLists;
in
{
  packageOverrides = pkgs: with pkgs; rec {
    inherit envLists env;
  };

  allowUnfree = true;
  # allowBroken = true;
     allowUnsupportedSystem = true;
}
