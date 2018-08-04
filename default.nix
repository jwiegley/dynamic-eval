{ compiler ? "ghc822"
, doStrict ? false
, rev      ? "d1ae60cbad7a49874310de91cd17708b042400c8"
, sha256   ? "0a1w4702jlycg2ab87m7n8frjjngf0cis40lyxm3vdwn7p4fxikz"
, pkgs     ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "hnix requires at least nix 2.0"
    else import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
           inherit sha256; }) {
           config.allowUnfree = true;
           config.allowBroken = false;
         }
, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let

haskellPackages = pkgs.haskell.packages.${compiler};

drv = haskellPackages.developPackage {
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
  };

  source-overrides = {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    configureFlags =
      pkgs.stdenv.lib.optional doStrict "--ghc-options=-Werror";
  });

  inherit returnShellEnv;
};

in drv
