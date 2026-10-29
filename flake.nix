{
  description = "usta: a native desktop client for code forges";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      haskellPackagesFor = pkgs: pkgs.haskell.packages.ghc912;

      # Written by hand instead of callCabal2nix: that runs cabal2nix at
      # evaluation time, built with the default GHC rather than ghc912.
      # Keep the dependencies in sync with usta.cabal.
      ustaFor = pkgs:
        let hsPkgs = haskellPackagesFor pkgs;
        in hsPkgs.mkDerivation {
          pname = "usta";
          version = "0.1.0.0";
          src = pkgs.lib.cleanSource ./.;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = with hsPkgs; [ base ];
          license = pkgs.lib.licenses.asl20;
          mainProgram = "usta";
        };
    in
    {
      packages = forAllSystems (pkgs: {
        usta = ustaFor pkgs;
        default = self.packages.${pkgs.system}.usta;
      });

      apps = forAllSystems (pkgs: {
        default = {
          type = "app";
          program = "${self.packages.${pkgs.system}.usta}/bin/usta";
        };
      });

      devShells = forAllSystems (pkgs:
        let hsPkgs = haskellPackagesFor pkgs;
        in {
          default = hsPkgs.shellFor {
            packages = _: [ (ustaFor pkgs) ];
            nativeBuildInputs = [ hsPkgs.cabal-install ];
          };
        });
    };
}
