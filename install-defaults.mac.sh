#!/usr/bin/env bash
# shellcheck disable=2088

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

# https://macos-defaults.com/
# https://github.com/kevinSuttle/macOS-Defaults/blob/master/REFERENCE.md
# https://github.com/mathiasbynens/dotfiles/blob/master/.macos

# TODO:
# https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/EventOverview/TextDefaultsBindings/TextDefaultsBindings.html
# http://yummymelon.com/devnull/an-accidental-lock-in-feature-of-the-apple-ecosystem.html
# defaults write NSGlobalDomain NSTextKillRingSize 5
# NSMnemonicsWorkInText
# https://brettterpstra.com/projects/keybindings/
# https://brettterpstra.com/2023/12/18/macos-keybinding-tricks-the-kill-ring/
# To see default keybindings:
#   plutil -convert xml1 -o - /System/Library/Frameworks/AppKit.framework/Resources/StandardKeyBinding.dict | bat --language xml

# https://github.com/8ta4/plist
# https://developer.okta.com/blog/2021/07/19/discover-macos-settings-with-plistwatch

bool_to_num() {
	case "$(echo "$1" | tr '[:upper:]' '[:lower:]')" in
		(true|yes|1) printf "1\n" ;;
		(false|no|0) printf "0\n" ;;
		(*) return 1 ;;
	esac
}

defaults_write() {
	if [[ "$3" == -bool* && "$(defaults read-type "$1" "$2")" == "Type is boolean" && "$(defaults read "$1" "$2")" == "$(bool_to_num "$4")" ]]; then
		return
	fi
	if [[ "$3" == -string && "$(defaults read-type "$1" "$2")" == "Type is string" && "$(defaults read "$1" "$2")" == "$4" ]]; then
		return
	fi
	if [[ "$3" == -int* && "$(defaults read-type "$1" "$2")" == "Type is integer" && "$(defaults read "$1" "$2")" == "$4" ]]; then
		return
	fi
	(
		set -x
		defaults write "$@"
	)
}

## ApplePressAndHold
# By default, holding down a key lets you open accents instead of
# repeating the key. But I want to try out vim keybindings, and I want
# to be able to repeat the hjkl keys while in VSCode.
defaults_write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false
# JetBrains WebStorm
defaults_write com.jetbrains.WebStorm ApplePressAndHoldEnabled -bool false
# To do it globally:
#defaults_write -g ApplePressAndHoldEnabled 0

## VSCode
# https://superuser.com/a/1274941
defaults_write com.microsoft.VSCode NSAutomaticCapitalizationEnabled -bool false
defaults_write com.microsoft.VSCode NSAutomaticDashSubstitutionEnabled -bool false
defaults_write com.microsoft.VSCode NSAutomaticPeriodSubstitutionEnabled -bool false
defaults_write com.microsoft.VSCode NSAutomaticQuoteSubstitutionEnabled -bool false
defaults_write com.microsoft.VSCode NSAutomaticSpellingCorrectionEnabled -bool false
defaults_write com.microsoft.VSCode NSAutomaticTextCompletionEnabled -bool false

## Emacs
defaults_write org.gnu.Emacs NSAutomaticCapitalizationEnabled -bool false
defaults_write org.gnu.Emacs NSAutomaticDashSubstitutionEnabled -bool false
defaults_write org.gnu.Emacs NSAutomaticPeriodSubstitutionEnabled -bool false # this is the one I was looking for
defaults_write org.gnu.Emacs NSAutomaticQuoteSubstitutionEnabled -bool false
defaults_write org.gnu.Emacs NSAutomaticSpellingCorrectionEnabled -bool false
defaults_write org.gnu.Emacs NSAutomaticTextCompletionEnabled -bool false

# https://github.com/rejeep/prodigy.el?tab=readme-ov-file#installation
defaults_write org.gnu.Emacs NSAppSleepDisabled -bool YES

# https://petar.dev/notes/enhancing-font-rendering-in-emacs-on-macos/
# defaults_write org.gnu.Emacs AppleFontSmoothing -int 0
# to undo: defaults delete org.gnu.Emacs AppleFontSmoothing
# also see: https://old.reddit.com/r/MacOS/comments/16tow2w/psa_turn_off_font_smoothing/

# https://stackoverflow.com/a/20291771
defaults_write org.gnu.Emacs NSQuitAlwaysKeepsWindows -bool false

## General
# https://macos-defaults.com/dock/autohide.html
defaults_write com.apple.dock "autohide" -bool "true" #&& killall Dock
# https://macos-defaults.com/finder/appleshowallextensions.html
defaults_write NSGlobalDomain "AppleShowAllExtensions" -bool "true" #&& killall Finder

# need to restart for this to take effect
# https://github.com/mathiasbynens/dotfiles/blob/c886e139233320e29fd882960ba3dd388d57afd7/.macos#L154
# https://gist.github.com/hofmannsven/ff21749b0e6afc50da458bebbd9989c5
# https://mac-key-repeat.zaymon.dev/
# Set a blazingly fast keyboard repeat rate
# These numbers are multiples of 15ms
defaults_write NSGlobalDomain InitialKeyRepeat -int 20 # before: 25; normal minimum is 15 (225 ms)
defaults_write NSGlobalDomain KeyRepeat        -int 5  # before: 6;  normal minimum is 2  (30 ms)

# Expand save panel by default
defaults_write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults_write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true
# Expand print panel by default
defaults_write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults_write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Save to disk (not to iCloud) by default
defaults_write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false
# Disable automatic capitalization as it’s annoying when typing code
defaults_write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false
# Disable smart dashes as they’re annoying when typing code
# defaults_write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false
# Disable automatic period substitution as it’s annoying when typing code
# defaults_write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false
# Disable smart quotes as they’re annoying when typing code
# defaults_write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# https://macos-defaults.com/screenshots/location.html
# shellcheck disable=SC2088
defaults_write com.apple.screencapture "location" -string "~/Downloads" #&& killall SystemUIServer

# Enable subpixel font rendering on non-Apple LCDs
# Reference: https://github.com/kevinSuttle/macOS-Defaults/issues/17#issuecomment-266633501
# possible values: 0-3
# defaults_write NSGlobalDomain AppleFontSmoothing -int 1

# Path for new Finder windows
# https://github.com/mathiasbynens/dotfiles/blob/c886e139233320e29fd882960ba3dd388d57afd7/.macos#L250
# https://apple.stackexchange.com/a/306212
# before (Recents):
# defaults read com.apple.finder NewWindowTarget     => PfAF
# defaults read com.apple.finder NewWindowTargetPath =>  file:///System/Library/CoreServices/Finder.app/Contents/Resources/MyLibraries/myDocuments.cannedSearch
defaults_write com.apple.finder NewWindowTarget -string "PfLo"
defaults_write com.apple.finder NewWindowTargetPath -string "file://$HOME/Downloads/"

# Disable the warning when changing a file extension
defaults_write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Avoid creating .DS_Store files on network or USB volumes
# defaults_write com.apple.desktopservices DSDontWriteNetworkStores -bool true
# defaults_write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Automatically open a new Finder window when a volume is mounted
# defaults_write com.apple.frameworks.diskimages auto-open-ro-root -bool true
# defaults_write com.apple.frameworks.diskimages auto-open-rw-root -bool true
# defaults_write com.apple.finder OpenWindowForNewRemovableDisk -bool true

# Show the ~/Library folder
chflags -vv nohidden ~/Library

# https://macos-defaults.com/activity-monitor/updateperiod.html
defaults_write com.apple.ActivityMonitor "UpdatePeriod" -int "2" #&& killall Activity\ Monitor
# https://macos-defaults.com/activity-monitor/icontype.html
defaults_write com.apple.ActivityMonitor IconType -int 5
# Show all processes in Activity Monitor
defaults_write com.apple.ActivityMonitor ShowCategory -int 0

## iTerm 2
# Specify the preferences directory
defaults_write com.googlecode.iterm2.plist PrefsCustomFolder -string "~/.config/iterm2"
# Tell iTerm2 to use the custom preferences in the directory
defaults_write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true

## Hammerspoon
# Put config in ~/.config/hammerspoon instead of ~/.hammerspoon
defaults_write org.hammerspoon.Hammerspoon MJConfigFile -string "~/.config/hammerspoon/init.lua"
