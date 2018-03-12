{
  packageOverrides = pkgs : with pkgs; rec {
    cEnv = myEnvFun {
      name = "cEnv";
      buildInputs = [ stdenv clang ];
    };

    nodeEnv = myEnvFun {
      name = "nodeEnv";
      buildInputs = [
        neovim
        zsh
        nodejs
        "tern"
      ];
    };

    # go = go_1_8;

    myHunspell = hunspellWithDicts (with hunspellDicts; [
      en-us
    ]);

    dev-env = buildEnv {
      name = "dev-env";
      paths = [
        stdenv
        clang
        curl
        bashInteractive
        diffutils
        #direnv
        dos2unix
        entr
        git
        #gitAndTools.hub
        httpie
        gnugrep
        gawk
        patch
        gnutar
        man
        xz
        zsh
        pkgconfig
        wget
      ];
    };

    misc-env = buildEnv {
      name = "misc-env";
      paths = [
        exercism
        imagemagick
        rsync
        smartmontools
      ];
    };

    emacs-env = buildEnv {
      name = "emacs-env";
      paths = [
        myHunspell
        emacs25
        gnupg
        nodePackages.tern
      ];
    };

    tmux-env = buildEnv {
      name = "tmux-env";
      paths = [
        tmux
      ] ++ lib.optional stdenv.isDarwin reattach-to-user-namespace;
    };

    elm-env = buildEnv {
      name = "elm-env";
      paths = [
        elmPackages.elm
      ];
    };

    latex-env = buildEnv {
      name = "latex-env";
      paths = [
        pandoc
        pdfpc
        pstoedit
        # epstool
        texinfo
      ];
    };

    vim-env = buildEnv {
      name = "vim-env";
      paths = [
        neovim
        # go
      ];
    };

    nix-env = buildEnv {
      name = "nix-env";
      paths = [
        nix-repl
        # nox
        #cabal2nix
        #nodePackages.node2nix
      ];
    };

    mKpython-env = { pythonPackages, version } :
    buildEnv {
      name = "python${version}-env";
      paths = [
        pythonPackages.python
        pythonPackages.jedi
        pythonPackages.ipython
        pythonPackages.pygments
      ];
    };

    python2-env = mKpython-env { pythonPackages = python2Packages; version = "2"; };
    python3-env = mKpython-env { pythonPackages = python3Packages; version = "3"; };

    js-env = buildEnv {
      name = "js-env";
      paths = [
        nodejs-9_x
      ];
    };

    hs-env = buildEnv {
      name = "hs-env";
      paths = with haskellPackages; [
        ghc
        hindent
        ghcid
        stylish-haskell
        intero
        hpack
        # hpack-convert
        hlint
        doctest
      ];
    };

    ruby-env = buildEnv {
      name = "ruby-env";
      paths = [
        ruby
      ];
    };

    rust-env = buildEnv {
      name = "rust-env";
      paths = [
        rustc
        rustfmt
        rustracer
        cargo
      ];
    };

    go-env = buildEnv {
      name = "go-env";
      paths = [
        go
        gocode
      ];
    };

    R-env = buildEnv {
      name = "R-env";
      paths = [
        R
        rPackages.tikzDevice
      ];
    };

    taskwarrior-env = buildEnv {
      name = "taskwarrior-env";
      paths = [
        taskwarrior
        vit
      ];
    };

    jupyter-env = buildEnv {
      name = "jupyter-env";
      paths = with pythonPackages; [
        (jupyter.override {
          propagatedBuildInputs = with self; [
            notebook
            qtconsole
            jupyter_console
            nbconvert
            ipykernel
            ipywidgets
          ];
        })
      ];
    };

    ocaml-env = buildEnv {
      name = "ocaml-env";
      paths = with ocaml-ng.ocamlPackages_4_05; [
        ocaml
        ocamlbuild
        merlin
        ocpIndent
        utop
      ];
    };

  };

  allowUnfree = true;
  allowBroken = true;
}
