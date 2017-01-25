{
  packageOverrides = pkgs : with pkgs; rec {
    cEnv = myEnvFun {
      name = "cEnv";
      buildInputs = [ stdenv clang ];
    };

    all = buildEnv {
      name = "all";
      paths = [
        stdenv
        clang
        elmPackages.elm
        (if stdenv.isDarwin then "emacs-25.1-mac-6.0" else emacs)
        (if stdenv.isDarwin then reattach-to-user-namespace else "")
        exercism
        gnupg
        go
        imagemagick
        (import ~/.nixpkgs/jupyter.nix)
        man
        neovim
        nodejs
        nox
        "nodePackages.nyaovim"
        "nodePackages.tern"
        python
        python3
        python3Packages.ipython
        ruby
        rustc
        rustfmt
        rustracer
        # stack
        texinfo
        tmux
        vim
        zsh
      ];
    };
  };

  allowUnfree = true;
}
