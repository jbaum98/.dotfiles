{ buildEnv, bash }:
{
  bash-env = buildEnv {
    name = "bash-env";

    paths = [ bash ];
  };
}
