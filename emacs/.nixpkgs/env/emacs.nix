{ buildEnv, emacs25, hunspellWithDicts, hunspellDicts}:
let
  myHunspell = hunspellWithDicts (with hunspellDicts; [
    en-us
  ]);
in {
  env.emacs = [
    myHunspell
    emacs25
  ];
}
