{ buildEnv, zsh }:
{
  zsh-env = buildEnv {
    name = "zsh-env";

    paths = [ zsh ];
  };
}
