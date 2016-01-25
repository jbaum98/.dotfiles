{
  packageOverrides = pkgs : with pkgs; rec {
    cEnv = pkgs.myEnvFun {
      name = "cEnv";
      buildInputs = [ stdenv clang ];
    };
  };
}
