{ sources, compiler }:
[
  (final: prev: {
    inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreFilter;
  })
  (final: prev: {
    aoc21 = import ./packages.nix { pkgs = prev; inherit compiler; };
  })
]
