{ buildEnv, emacs25, hunspellWithDicts, hunspellDicts, proselint}:
let
  myHunspell = hunspellWithDicts (with hunspellDicts; [
    en-us
  ]);
in {
  env.emacs = [
    myHunspell
    emacs25
    proselint
  ];
}
