#!/usr/bin/env bash
# Builds usta.app and usta.dmg in a folder (dist/ by default) from the
# nix build: the binary, every library it loads from /nix/store copied
# into Contents/Frameworks, and GTK's schemas, icons and image loaders,
# so the app runs on a Mac without nix.
#
# USTA_SIGN_IDENTITY names a Developer ID certificate in the keychain to
# sign with (hardened runtime); without it the app is signed ad hoc, which
# runs here but makes Gatekeeper ask elsewhere.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
out=${1:-$root/dist}
identity=${USTA_SIGN_IDENTITY:--}

pkg=$(nix build "$root#usta" --no-link --print-out-paths)
data=$(nix build "$root#bundle-data" --no-link --print-out-paths)
version=$(sed -n 's/^version: *//p' "$root/usta.cabal")

app=$out/usta.app
contents=$app/Contents
rm -rf "$app" "$out/usta.dmg"
mkdir -p "$contents/MacOS" "$contents/Frameworks" "$contents/Resources"

# What GTK reads at run time; the loaders are libraries too.
cp -R "$data/share" "$data/lib" "$contents/Resources/"
chmod -R u+w "$contents/Resources"
loaders=$contents/Resources/lib/gdk-pixbuf-2.0/2.10.0/loaders

cp "$pkg/bin/.usta-wrapped" "$contents/MacOS/usta-bin"
chmod u+w "$contents/MacOS/usta-bin"

# Every /nix/store library a file loads, copied once into Frameworks and
# loaded from there. Two packages can ship a library of the same name
# (libiconv does), so a name already taken gets the store hash in front.
copied=$(mktemp)
store_libs() {
  otool -L "$1" | awk 'NR > 1 {print $1}' | grep '^/nix/store/' || true
}
name_for() {
  local lib=$1 name
  name=$(awk -v p="$lib" '$1 == p {print $2}' "$copied")
  if [ -z "$name" ]; then
    name=$(basename "$lib")
    if awk -v n="$name" '$2 == n {found = 1} END {exit !found}' "$copied"; then
      name=$(echo "$lib" | cut -d/ -f4 | cut -c1-8)-$name
    fi
  fi
  echo "$name"
}
relink() {
  local file=$1
  local lib
  for lib in $(store_libs "$file"); do
    local name
    name=$(name_for "$lib")
    install_name_tool -change "$lib" "@executable_path/../Frameworks/$name" "$file" 2>/dev/null
    if ! awk -v p="$lib" '$1 == p {found = 1} END {exit !found}' "$copied"; then
      echo "$lib $name" >> "$copied"
      cp "$lib" "$contents/Frameworks/$name"
      chmod u+w "$contents/Frameworks/$name"
      install_name_tool -id "@executable_path/../Frameworks/$name" "$contents/Frameworks/$name" 2>/dev/null
      relink "$contents/Frameworks/$name"
    fi
  done
}
relink "$contents/MacOS/usta-bin"
for loader in "$loaders"/*.so; do
  install_name_tool -id "@loader_path/$(basename "$loader")" "$loader" 2>/dev/null
  relink "$loader"
done

# The loaders' list names them by where the bundle is, which only the
# launcher knows.
mv "$contents/Resources/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache.in" "$contents/Resources/loaders.cache.in"
cat > "$contents/MacOS/usta" <<'LAUNCHER'
#!/bin/sh
# Points GTK at the bundle's own data, then runs usta.
contents=$(cd "$(dirname "$0")/.." && pwd)
resources=$contents/Resources
export XDG_DATA_DIRS="$resources/share${XDG_DATA_DIRS:+:$XDG_DATA_DIRS}"
export GSETTINGS_SCHEMA_DIR="$resources/share/glib-2.0/schemas"
cache="$HOME/Library/Caches/usta/loaders.cache"
mkdir -p "$(dirname "$cache")"
sed "s|@LOADERS@|$resources/lib/gdk-pixbuf-2.0/2.10.0/loaders|" "$resources/loaders.cache.in" > "$cache"
export GDK_PIXBUF_MODULE_FILE="$cache"
exec "$contents/MacOS/usta-bin" "$@"
LAUNCHER
chmod +x "$contents/MacOS/usta"

sed "s/@VERSION@/$version/g" "$root/data/macos/Info.plist" > "$contents/Info.plist"

# The icon: the logo with macOS's room around it, at every size.
iconset=$(mktemp -d)/usta.iconset
mkdir -p "$iconset"
big=$(dirname "$iconset")/usta-1024.png
cp "$data/icon-824.png" "$big"
chmod u+w "$big"
sips --padToHeightWidth 1024 1024 "$big" > /dev/null
for size in 16 32 128 256 512; do
  sips -z "$size" "$size" "$big" --out "$iconset/icon_${size}x${size}.png" > /dev/null
  sips -z $((size * 2)) $((size * 2)) "$big" --out "$iconset/icon_${size}x${size}@2x.png" > /dev/null
done
iconutil -c icns "$iconset" -o "$contents/Resources/usta.icns"

# Every changed library needs a new signature; then the whole bundle.
if [ "$identity" = "-" ]; then
  sign=(codesign --force --sign -)
else
  sign=(codesign --force --timestamp --options runtime --entitlements "$root/data/macos/entitlements.plist" --sign "$identity")
fi
for file in "$contents/Frameworks"/* "$loaders"/*.so "$contents/MacOS/usta-bin"; do
  "${sign[@]}" "$file"
done
"${sign[@]}" "$app"

hdiutil create -quiet -volname usta -srcfolder "$app" -ov -format UDZO "$out/usta.dmg"
echo "$app"
echo "$out/usta.dmg"
