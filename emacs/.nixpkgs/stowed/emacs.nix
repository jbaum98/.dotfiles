{ buildEnv, emacs25, hunspellWithDicts, hunspellDicts}:
let
  myHunspell = hunspellWithDicts (with hunspellDicts; [
    en-us
  ]);
in {
  emacs-env = buildEnv {
    name = "emacs-env";
    paths = [
      myHunspell
      emacs25
    ];
  };
}
