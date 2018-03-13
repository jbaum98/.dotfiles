{ clang, curl, coreutils, diffutils, direnv, dos2unix, entr,
  httpie, gnugrep, ripgrep, gawk, patch, gnutar, man, xz }:
{
  env.default = [
    clang curl coreutils diffutils direnv dos2unix entr
    httpie gnugrep ripgrep gawk patch gnutar man xz
  ];
}
