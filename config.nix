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
        aspell
        clang
        curl
        direnv
        dos2unix
        # epstool
        elmPackages.elm
        (if stdenv.isDarwin then "emacs-25.1-mac-6.0" else emacs)
        (if stdenv.isDarwin then reattach-to-user-namespace else "")
        exercism
        gawk
        gnupg
        go
        gocode
        gnugrep
        gitAndTools.hub
        httpie
        imagemagick
        pandoc
        pythonPackages.jedi
        (import ~/.nixpkgs/jupyter.nix)
        pstoedit
        man
        neovim
        nodejs
        nox
        "nodePackages.nyaovim"
        #"nodePackages.tern"
        python
        python3
        python3Packages.ipython
        ruby
        rustc
        rustfmt
        rustracer
        # stack
        taskwarrior
        texinfo
        tmux
        vim
        xz
        zsh
      ];
    };
  };

  allowUnfree = true;
  # allowBroken = true;
}
