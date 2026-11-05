{
  description = "usta: a native desktop client for code forges";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

    haskellPackagesFor = pkgs:
      pkgs.haskell.packages.ghc912.override {
        overrides = _: super: {
          # Its tests need the mysql package, whose Setup.hs fails on
          # GHC 9.12 on Linux; usta needs only the library.
          esqueleto = pkgs.haskell.lib.dontCheck super.esqueleto;
        };
      };

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
        executableHaskellDepends = with hsPkgs; [
          aeson
          async
          base
          bytestring
          cmark-gfm
          containers
          directory
          emojis
          esqueleto
          filepath
          gi-cairo
          gi-cairo-connector
          gi-cairo-render
          gi-gdk4
          gi-gio
          gi-glib
          gi-gobject
          gi-graphene
          gi-gtk4
          gi-pango
          gi-pangocairo
          gi-poppler
          haskell-gi-base
          http-api-data
          http-client
          http-client-tls
          http-types
          monad-logger
          persistent
          persistent-sqlite
          resourcet
          servant
          servant-client
          servant-client-core
          servant-multipart-api
          servant-multipart-client
          skylighting
          stm
          text
          time
          typed-process
        ];
        # The keyring: Keychain Services ship with macOS's SDK.
        executablePkgconfigDepends = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [pkgs.libsecret];
        license = pkgs.lib.licenses.asl20;
        mainProgram = "usta";
      }).overrideAttrs (old: {
        # Lets the installed binary find GTK's schemas and the icon theme.
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.wrapGAppsHook4];
        buildInputs = (old.buildInputs or []) ++ [pkgs.gtk4 pkgs.adwaita-icon-theme];
        # On Linux the desktop entry (which claims usta:// links), the
        # AppStream data and the icon.
        postInstall =
          (old.postInstall or "")
          + pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
            install -Dm644 data/linux/io.github.abdivasiyev.Usta.desktop $out/share/applications/io.github.abdivasiyev.Usta.desktop
            install -Dm644 data/linux/io.github.abdivasiyev.Usta.metainfo.xml $out/share/metainfo/io.github.abdivasiyev.Usta.metainfo.xml
            install -Dm644 data/usta.svg $out/share/icons/hicolor/scalable/apps/io.github.abdivasiyev.Usta.svg
          '';
      });
  in {
    packages = forAllSystems (pkgs: {
      usta = ustaFor pkgs;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.usta;
      # What GTK reads at run time, for scripts/macos-app.sh to put in the
      # bundle: compiled settings schemas, the SVG image loader and the
      # Adwaita icons.
      # The image loaders: gdk-pixbuf's own and librsvg's SVG one.
      bundle-data = pkgs.runCommand "usta-bundle-data" {nativeBuildInputs = [pkgs.glib.dev pkgs.librsvg];} ''
        mkdir -p $out/share/glib-2.0/schemas $out/share/icons $out/lib/gdk-pixbuf-2.0/2.10.0/loaders
        # The app icon's art, on macOS's grid: 824 of its 1024 pixels.
        rsvg-convert -w 824 -h 824 ${./data/usta.svg} -o $out/icon-824.png
        cp ${pkgs.gtk4}/share/gsettings-schemas/${pkgs.gtk4.name}/glib-2.0/schemas/*.xml $out/share/glib-2.0/schemas/
        cp ${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}/glib-2.0/schemas/*.xml $out/share/glib-2.0/schemas/
        glib-compile-schemas $out/share/glib-2.0/schemas
        cp -r ${pkgs.adwaita-icon-theme}/share/icons/Adwaita $out/share/icons/
        cp -r ${pkgs.hicolor-icon-theme}/share/icons/hicolor $out/share/icons/
        cp ${pkgs.gdk-pixbuf}/lib/gdk-pixbuf-2.0/2.10.0/loaders/* ${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders/* $out/lib/gdk-pixbuf-2.0/2.10.0/loaders/
        sed -e "s|${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders|@LOADERS@|" -e "s|${pkgs.gdk-pixbuf}/lib/gdk-pixbuf-2.0/2.10.0/loaders|@LOADERS@|" \
          ${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache > $out/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache.in
      '';
    });

    apps = forAllSystems (pkgs: {
      default = {
        type = "app";
        program = "${self.packages.${pkgs.stdenv.hostPlatform.system}.usta}/bin/usta";
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
          # The checks from CONTRIBUTING.md run on every commit.
          if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
            git config core.hooksPath .githooks
          fi
        '';
      };
    });
  };
}
