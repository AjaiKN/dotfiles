#!/usr/bin/env bash

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

set -x

# https://github.com/houmain/keymapper?tab=readme-ov-file#building

sudo dnf -y install git cmake make gcc-c++
sudo dnf -y install libudev-devel libusb1-devel dbus-devel
# optional for Wayland support
sudo dnf -y install wayland-devel libxkbcommon-devel
# optional for X11 support
sudo dnf -y install libX11-devel
# optional for tray icon support
sudo dnf -y install libappindicator-gtk3-devel

if ! [ -e ~/.local/build/keymapper ]; then
	mkdir -p ~/.local/build
	cd ~/.local/build
	git clone --depth=1 https://github.com/houmain/keymapper
fi

cd ~/.local/build/keymapper

cmake -B build
cmake --build build -j4
sudo cmake --install build

# keymapperd service
sudo systemctl daemon-reload
sudo systemctl start --now keymapperd
sudo systemctl enable keymapperd
# keymapper service
systemctl --user daemon-reload
systemctl --user start --now keymapper
systemctl --user enable keymapper
