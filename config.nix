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
        diffutils
        direnv
        dos2unix
        # epstool
        entr
        elmPackages.elm
        (if stdenv.isDarwin then emacs25Macport else emacs)
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
        patch
        pandoc
        pdfpc
        pythonPackages.jedi
        #(import ~/.nixpkgs/jupyter.nix)
        pstoedit
        man
        neovim
        nix-repl
        nodejs
        cabal-install
        cabal2nix
        nodePackages.node2nix
        nox
        "nodePackages.nyaovim"
        pkgconfig
        #"nodePackages.tern"
        python
        python3
        python3Packages.ipython
        python3Packages.pygments
        R
        rPackages.tikzDevice
        rsync
        ruby
        rustc
        rustfmt
        rustracer
        smartmontools
        stack
        taskwarrior
        texinfo
        tmux
        vim
        vit
        xz
        zsh
      ];
    };
  };

  allowUnfree = true;
  # allowBroken = true;
}
