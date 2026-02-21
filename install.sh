#!/usr/bin/env bash
set -euo pipefail

APP_NAME="ada_orbit_vision"
INSTALL_DIR="$HOME/.local/share/$APP_NAME"
BIN_DIR="$HOME/.local/bin"
DESKTOP_DIR="$HOME/.local/share/applications"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# ── Dependency check ──────────────────────────────────────────────
check_dep() {
   if ! command -v "$1" &>/dev/null; then
      echo "ERROR: '$1' not found. $2"
      exit 1
   fi
}

check_lib() {
   if ! ldconfig -p 2>/dev/null | grep -q "$1"; then
      echo "ERROR: $1 not found. Install it via: sudo apt install $2"
      exit 1
   fi
}

echo "=== Ada-Orbit-Vision Installer ==="
echo ""

check_dep alr      "Install Alire: https://alire.ada.dev"
check_dep gnat     "Install GNAT: sudo apt install gnat"
check_lib libSDL2  libsdl2-dev
check_lib libSDL2_image libsdl2-image-dev
check_lib libSDL2_ttf  libsdl2-ttf-dev

echo "[1/4] Building project (release mode)..."
cd "$SCRIPT_DIR"
alr build --release

echo "[2/4] Installing binary..."
mkdir -p "$BIN_DIR"
cp bin/$APP_NAME "$BIN_DIR/$APP_NAME"
chmod +x "$BIN_DIR/$APP_NAME"

echo "[3/4] Installing assets and data..."
mkdir -p "$INSTALL_DIR"
[ -d assets ] && cp -r assets "$INSTALL_DIR/"
[ -d data ]   && cp -r data   "$INSTALL_DIR/"

echo "[4/4] Creating desktop entry..."
mkdir -p "$DESKTOP_DIR"
cat > "$DESKTOP_DIR/$APP_NAME.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=Ada Orbit Vision
Comment=Satellite orbital simulation and image processing
Exec=$BIN_DIR/$APP_NAME
Path=$INSTALL_DIR
Icon=$INSTALL_DIR/assets/icon.png
Terminal=false
Categories=Science;Education;
StartupNotify=true
EOF
chmod +x "$DESKTOP_DIR/$APP_NAME.desktop"

# Refresh desktop database if available
if command -v update-desktop-database &>/dev/null; then
   update-desktop-database "$DESKTOP_DIR" 2>/dev/null || true
fi

echo ""
echo "=== Installation complete ==="
echo "Binary:  $BIN_DIR/$APP_NAME"
echo "Data:    $INSTALL_DIR/"
echo "Desktop: $DESKTOP_DIR/$APP_NAME.desktop"
echo ""
echo "You can now launch Ada-Orbit-Vision from your application menu."
echo "Make sure ~/.local/bin is in your PATH."
