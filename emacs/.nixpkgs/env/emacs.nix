{ lib, stdenv, callPackage, emacs, emacsMacport, hunspellWithDicts, hunspellDicts, proselint}:
let
  myHunspell = hunspellWithDicts (with hunspellDicts; [
    en-us
  ]);

  emacsPrefetch = callPackage ./nix-emacs { emacs = if stdenv.isDarwin then emacsMacport else emacs; };

in {
  env.emacs = [
    myHunspell
    (emacsPrefetch ~/.dotfiles/emacs.d)
    proselint
  ];
}
