{s}: rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:quickservant' --allow-eval --warnings";
  allScripts = [ghcidScript];
}
