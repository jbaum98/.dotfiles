{
  packageOverrides = pkgs : with pkgs; rec {
    cEnv = myEnvFun {
      name = "cEnv";
      buildInputs = [ stdenv clang ];
    };

    all = buildEnv {
      name = "all";
      paths = [
        nox
        (if stdenv.isDarwin then "emacs-25.1-mac-6.0" else emacs)
        neovim
        vim
        rustc
        rustfmt
        rustracer
        go
        python
        python3
        python3Packages.ipython
        (import ~/.nixpkgs/jupyter.nix)
        ruby
        clang
        # stack
        nodejs
        "nodePackages.tern"
        gnupg
        zsh
        tmux
      ];
    };
  };

  allowUnfree = true;
}
