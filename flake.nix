{
  description = "usta: a native desktop client for code forges";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

    haskellPackagesFor = pkgs: pkgs.haskell.packages.ghc912;

    # Written by hand instead of callCabal2nix: that runs cabal2nix at
    # evaluation time, built with the default GHC rather than ghc912.
    # Keep the dependencies in sync with usta.cabal.
    ustaFor = pkgs: let
      hsPkgs = haskellPackagesFor pkgs;
    in
      (hsPkgs.mkDerivation {
        pname = "usta";
        version = "0.1.0.0";
        src = pkgs.lib.cleanSource ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = with hsPkgs; [base gi-gdk4 gi-glib gi-gtk4 gi-pango haskell-gi-base text];
        license = pkgs.lib.licenses.asl20;
        mainProgram = "usta";
      }).overrideAttrs (old: {
        # Lets the installed binary find GTK's schemas and the icon theme.
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.wrapGAppsHook4];
        buildInputs = (old.buildInputs or []) ++ [pkgs.gtk4 pkgs.adwaita-icon-theme];
      });
  in {
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

    devShells = forAllSystems (pkgs: let
      hsPkgs = haskellPackagesFor pkgs;
    in {
      default = hsPkgs.shellFor {
        packages = _: [(ustaFor pkgs)];
        nativeBuildInputs = [
          hsPkgs.cabal-install
          hsPkgs.haskell-language-server
          hsPkgs.cabal-gild
        ];
        buildInputs = [pkgs.adwaita-icon-theme];
        shellHook = ''
          export XDG_DATA_DIRS="${pkgs.adwaita-icon-theme}/share:${pkgs.gtk4}/share/gsettings-schemas/${pkgs.gtk4.name}''${XDG_DATA_DIRS:+:$XDG_DATA_DIRS}"
        '';
      };
    });
  };
}
