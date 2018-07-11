{ clang, curl, coreutils, diffutils, dos2unix, entr,
  httpie, gnugrep, ripgrep, gawk, patch, gnutar, man, wget, xz }:
{
  env.default = [
    curl coreutils diffutils dos2unix entr
    httpie gnugrep ripgrep gawk patch gnutar man wget xz
  ];
}
