{ haskellPackages }:
{
  env.hs = with haskellPackages; [ 
    hpack
    cabal2nix
    cabal-install
  ];
}
