{ coq, emacsPackages }:
{
  env.coq = [ coq emacsPackages.proofgeneral ];
}
