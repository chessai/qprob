{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  qprob = (
    with rec {
      qprobSource = pkgs.lib.cleanSource ../.;
      qprobBasic  = self.callCabal2nix "qprob" qprobSource { };
    };
    overrideCabal qprobBasic (old: {
    })
  );
}
