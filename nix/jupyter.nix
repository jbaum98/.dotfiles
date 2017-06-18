{ stdenv, pkgs, rWrapper, rPackages, python27Packages, writeText, python35Packages, buildEnv }:
let deps = rec {
  builder = builtins.toFile "builder.sh" ''
  source $stdenv/setup
  mkdir -p $out
  cat > $out/kernel.json << EOF
  $json
  EOF
  '';

  python27 = pkgs.python27.buildEnv.override {
    extraLibs = with python27Packages; [
      # Kernel34
      ipykernel
      ipywidgets
      # Custom packages
      beautifulsoup4
      #     bokeh
      #     cloudpickle
      cython
      dill
      lightning
      matplotlib
      numba
      numpy
      pandas
      patsy
      pillow
      #     scikitimage
      #     scikitlearn
      scipy
      seaborn
      #     statsmodels
      #     sympy
    ];
  };

  python27_kernel = stdenv.mkDerivation rec {
    name = "python27";
    buildInputs = [ python27 ];
    json = builtins.toJSON {
      argv = [ "${python27}/bin/python2.7"
      "-m" "ipykernel" "-f" "{connection_file}" ];
      display_name = "Python 2.7";
      language = "python";
      env = { PYTHONPATH = ""; };
    };
    inherit builder;
  };

  #R = rWrapper.override {
  #  packages = with rPackages; [
  #    # Kernel
  #    rzmq
  #    repr
  #    IRkernel
  #    IRdisplay
  #    # Custom packages
  #    tm
  #    wordcloud
  #  ];
  #};
  #R_kernel = stdenv.mkDerivation rec {
  #  name = "ir";
  #  buildInputs = [ R ];
  #  json = builtins.toJSON {
  #    argv = [ "${R}/bin/R"
  #    "--slave" "-e" "IRkernel::main()"
  #    "--args" "{connection_file}" ];
  #    display_name = "R";
  #    language = "R";
  #  };
  #  inherit builder;
  #};

  jupyter_config_dir = stdenv.mkDerivation {
    name = "jupyter_config_dir";
    buildInputs = [
      # R_kernel
      python27_kernel
    ];

    builder = writeText "builder.sh" ''
    source $stdenv/setup
    mkdir -p $out/kernels $out/migrated
    ln -s ${python27_kernel} $out/kernels/${python27_kernel.name}
    cat > $out/jupyter_notebook_config.py << EOF
    c.KernelSpecManager.whitelist = {
    '${python27_kernel.name}'
    }
    EOF
    '';
    # '${R_kernel.name}',
    # ln -s ${R_kernel} $out/kernels/${R_kernel.name}
  };

  jupyter_app = python35Packages.notebook.override {
    postInstall = with python35Packages; ''
    mkdir -p $out/bin
    ln -s ${jupyter_core}/bin/* $out/bin
    ln -s ${jupyter_client}/bin/* $out/bin
    ln -s ${jupyter_console}/bin/* $out/bin
    wrapProgram $out/bin/jupyter \
    --prefix PYTHONPATH : "${notebook}/lib/python3.5/site-packages:$PYTHONPATH" \
    --prefix PATH : "${notebook}/bin:$PATH" \
    --prefix JUPYTER_PATH : "${jupyter_config_dir}" \
    --prefix JUPYTER_CONFIG_DIR : "${jupyter_config_dir}"
    '';
  };
};
in with deps; 
stdenv.mkDerivation rec {
  name = "jupyter";
  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
  source $stdenv/setup; ln -s $env $out
  '';
  buildInputs = [
    jupyter_app
    jupyter_config_dir
  ];
}
