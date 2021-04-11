{ sources ? import ./nix/sources.nix, }:
let
  pkgs = import sources.nixpkgs {};
  gis = import sources.nix-git-ignore-source {};
in
with pkgs;
rustPlatform.buildRustPackage {
  name = "rust-pager";
  version = "0.2.4";
  src = gis.gitIgnoreSource ./.;
  cargoSha256 = "1pd4313mlgsjy3w173gz7sry1l1a3p6ri8vh2ns297wyy795pda0";
  doCheck = false;
  nativeBuildInput = [ rustc cargo ];
}

