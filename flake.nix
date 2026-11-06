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
    # The icon themes for the wrapper's XDG_DATA_DIRS.
    iconThemesFor = pkgs: ''
      gappsWrapperArgs+=(--prefix XDG_DATA_DIRS : "${pkgs.adwaita-icon-theme}/share" --prefix XDG_DATA_DIRS : "${pkgs.hicolor-icon-theme}/share")
    '';
    ustaFor = pkgs: let
      iconThemes = iconThemesFor pkgs;
      hsPkgs = haskellPackagesFor pkgs;
    in
      (hsPkgs.mkDerivation {
        pname = "usta";
        version = "0.1.1";
        src = pkgs.lib.cleanSource ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = with hsPkgs; [
          aeson
          file-embed
          yaml
          scientific
          vector
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
        executableHaskellDepends = with hsPkgs; [
          base
          directory
          filepath
          gi-gio
          gi-gtk4
          haskell-gi-base
          http-client
          http-client-tls
          persistent
          text
          time
        ];
        testHaskellDepends = with hsPkgs; [
          aeson
          aeson-pretty
          base
          base64-bytestring
          bytestring
          case-insensitive
          containers
          cryptohash-sha256
          directory
          filepath
          gi-gdk4
          gi-gio
          gi-glib
          gi-graphene
          gi-gsk
          gi-gtk4
          gi-pango
          haskell-gi-base
          hedgehog
          http-client
          http-types
          network
          persistent
          servant-client
          tasty
          tasty-golden
          tasty-hedgehog
          tasty-hunit
          temporary
          text
          time
          typed-process
          vector
          wai
          warp
        ];
        benchmarkHaskellDepends = with hsPkgs; [
          base
          containers
          filepath
          persistent
          tasty-bench
          temporary
          text
          time
        ];
        # The tests make git repositories and mirror them.
        testToolDepends = [pkgs.git];
        # The UI tests need a display; the build runs the others.
        testTargets = ["usta-test"];
        # The keyring: Keychain Services ship with macOS's SDK.
        libraryPkgconfigDepends = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [pkgs.libsecret];
        license = pkgs.lib.licenses.asl20;
        mainProgram = "usta";
      }).overrideAttrs (old: {
        # Lets the installed binary find GTK's schemas and the icon theme.
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.wrapGAppsHook4];
        buildInputs = (old.buildInputs or []) ++ [pkgs.gtk4 pkgs.adwaita-icon-theme];
        # wrapGAppsHook adds schemas, not icon themes: without these the
        # icons GTK does not carry itself show as missing.
        preFixup = (old.preFixup or "") + iconThemes;
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
    # On macOS, usta.app around the nix build, for Home Manager and
    # nix-darwin to put in Applications: macOS opens usta:// links and
    # shows notifications only for an app. Unlike scripts/macos-app.sh's,
    # it loads everything from /nix/store. bin/usta is there too.
    ustaAppFor = pkgs: let
      usta = ustaFor pkgs;
      iconThemes = iconThemesFor pkgs;
    in
      pkgs.stdenvNoCC.mkDerivation {
        pname = "usta-app";
        inherit (usta) version;
        dontUnpack = true;
        nativeBuildInputs = [pkgs.wrapGAppsHook4 pkgs.makeShellWrapper pkgs.librsvg pkgs.libicns];
        buildInputs = [pkgs.gtk4 pkgs.adwaita-icon-theme pkgs.gsettings-desktop-schemas pkgs.librsvg];
        dontWrapGApps = true;
        preFixup = iconThemes;
        installPhase = ''
          contents=$out/Applications/usta.app/Contents
          mkdir -p $contents/MacOS $contents/Resources $out/bin
          # Linked, not copied: the binary runs from inside the bundle, so
          # macOS takes it for the app.
          ln -s ${usta}/bin/.usta-wrapped $contents/MacOS/usta-bin
          sed "s/@VERSION@/${usta.version}/g" ${./data/macos/Info.plist} > $contents/Info.plist
          # The icon: the logo with macOS's room around it, 824 of 1024.
          for size in 16 32 128 256 512 1024; do
            art=$((size * 824 / 1024)); edge=$(((size - art) / 2))
            rsvg-convert -w $art -h $art --page-width $size --page-height $size --left $edge --top $edge \
              ${./data/usta.svg} -o icon_$size.png
          done
          png2icns $contents/Resources/usta.icns icon_*.png
          ln -s ${usta}/bin/usta $out/bin/usta
        '';
        postFixup = ''
          makeShellWrapper $out/Applications/usta.app/Contents/MacOS/usta-bin $out/Applications/usta.app/Contents/MacOS/usta "''${gappsWrapperArgs[@]}"
        '';
        meta.mainProgram = "usta";
      };
  in {
    # pkgs.usta: the app on macOS, the program elsewhere.
    overlays.default = final: _: {
      usta =
        if final.stdenv.hostPlatform.isDarwin
        then ustaAppFor final
        else ustaFor final;
    };

    homeManagerModules.default = import ./nix/home-manager.nix self;
    nixosModules.default = import ./nix/system.nix self;
    darwinModules.default = import ./nix/system.nix self;

    packages = forAllSystems (pkgs: {
      usta = ustaFor pkgs;
      default =
        if pkgs.stdenv.hostPlatform.isDarwin
        then self.packages.${pkgs.stdenv.hostPlatform.system}.usta-app
        else self.packages.${pkgs.stdenv.hostPlatform.system}.usta;
      # What GTK reads at run time, for scripts/macos-app.sh to put in the
      # bundle: compiled settings schemas, the SVG image loader and the
      # Adwaita icons.
      # The image loaders: gdk-pixbuf's own and librsvg's SVG one.
      usta-app = ustaAppFor pkgs;
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

    # Smoke tests of what is shipped: `nix flake check`. The package's
    # own build runs the test suite.
    checks = forAllSystems (pkgs: let
      usta = ustaFor pkgs;
      inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
    in
      {
        # The installed program starts, and is the version it says.
        runs = pkgs.runCommand "usta-runs" {} ''
          ${usta}/bin/usta --version | grep -qx "usta ${usta.version}"
          touch $out
        '';
      }
      // pkgs.lib.optionalAttrs isLinux {
        # The desktop entry and the AppStream data are valid.
        desktop = pkgs.runCommand "usta-desktop" {nativeBuildInputs = [pkgs.desktop-file-utils pkgs.appstream];} ''
          desktop-file-validate ${usta}/share/applications/io.github.abdivasiyev.Usta.desktop
          appstreamcli validate --no-net ${usta}/share/metainfo/io.github.abdivasiyev.Usta.metainfo.xml
          test -f ${usta}/share/icons/hicolor/scalable/apps/io.github.abdivasiyev.Usta.svg
          touch $out
        '';
      }
      // pkgs.lib.optionalAttrs isDarwin {
        # usta.app has its icon, its version, and a program that starts.
        app = let
          app = ustaAppFor pkgs;
        in
          pkgs.runCommand "usta-app" {nativeBuildInputs = [pkgs.python3];} ''
            contents=${app}/Applications/usta.app/Contents
            test -s $contents/Resources/usta.icns
            python3 -c "import plistlib, sys; p = plistlib.load(open(sys.argv[1], 'rb')); assert p['CFBundleShortVersionString'] == sys.argv[2], p; assert p['CFBundleExecutable'] == 'usta', p" \
              $contents/Info.plist ${usta.version}
            $contents/MacOS/usta --version | grep -qx "usta ${usta.version}"
            touch $out
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
