#!/usr/bin/env bash

cd "$(dirname "$0")" || exit 1

sudo nixos-generate-config --dir .
# https://nixos.wiki/wiki/Nixos-rebuild
# sudo nixos-rebuild switch -I nixos-config="$(realpath ./configuration.nix)" --use-remote-sudo
sudo nixos-rebuild switch --flake . -I nixos-config="$(realpath ./configuration.nix)" --use-remote-sudo
