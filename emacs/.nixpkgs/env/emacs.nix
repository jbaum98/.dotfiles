{ callPackage, buildEnv, emacs, emacsMacport, hunspellWithDicts, hunspellDicts, proselint}:
let
  myHunspell = hunspellWithDicts (with hunspellDicts; [
    en-us
  ]);

  emacsPrefetch = callPackage ~/nix-emacs { emacs = if isDarwin then emacsMacport else emacs; };

in {
  env.emacs = [
    myHunspell
    (emacsPrefetch )
    proselint
  ];
}
