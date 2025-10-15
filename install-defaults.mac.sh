#!/usr/bin/env bash
# shellcheck disable=2088

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
set -x

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

## ApplePressAndHold
# By default, holding down a key lets you open accents instead of
# repeating the key. But I want to try out vim keybindings, and I want
# to be able to repeat the hjkl keys while in VSCode.
defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false
# JetBrains WebStorm
defaults write com.jetbrains.WebStorm ApplePressAndHoldEnabled -bool false
# To do it globally:
#defaults write -g ApplePressAndHoldEnabled 0

## VSCode
# https://superuser.com/a/1274941
defaults write com.microsoft.VSCode NSAutomaticCapitalizationEnabled -bool false
defaults write com.microsoft.VSCode NSAutomaticDashSubstitutionEnabled -bool false
defaults write com.microsoft.VSCode NSAutomaticPeriodSubstitutionEnabled -bool false
defaults write com.microsoft.VSCode NSAutomaticQuoteSubstitutionEnabled -bool false
defaults write com.microsoft.VSCode NSAutomaticSpellingCorrectionEnabled -bool false
defaults write com.microsoft.VSCode NSAutomaticTextCompletionEnabled -bool false

## Emacs
defaults write org.gnu.Emacs NSAutomaticCapitalizationEnabled -bool false
defaults write org.gnu.Emacs NSAutomaticDashSubstitutionEnabled -bool false
defaults write org.gnu.Emacs NSAutomaticPeriodSubstitutionEnabled -bool false # this is the one I was looking for
defaults write org.gnu.Emacs NSAutomaticQuoteSubstitutionEnabled -bool false
defaults write org.gnu.Emacs NSAutomaticSpellingCorrectionEnabled -bool false
defaults write org.gnu.Emacs NSAutomaticTextCompletionEnabled -bool false

# https://github.com/rejeep/prodigy.el?tab=readme-ov-file#installation
defaults write org.gnu.Emacs NSAppSleepDisabled -bool YES

# https://petar.dev/notes/enhancing-font-rendering-in-emacs-on-macos/
# defaults write org.gnu.Emacs AppleFontSmoothing -int 0
# to undo: defaults delete org.gnu.Emacs AppleFontSmoothing
# also see: https://old.reddit.com/r/MacOS/comments/16tow2w/psa_turn_off_font_smoothing/

# https://stackoverflow.com/a/20291771
defaults write org.gnu.Emacs NSQuitAlwaysKeepsWindows -bool false

## General
# https://macos-defaults.com/dock/autohide.html
defaults write com.apple.dock "autohide" -bool "true" #&& killall Dock
# https://macos-defaults.com/finder/appleshowallextensions.html
defaults write NSGlobalDomain "AppleShowAllExtensions" -bool "true" #&& killall Finder

# need to restart for this to take effect
# https://github.com/mathiasbynens/dotfiles/blob/c886e139233320e29fd882960ba3dd388d57afd7/.macos#L154
# https://gist.github.com/hofmannsven/ff21749b0e6afc50da458bebbd9989c5
# https://mac-key-repeat.zaymon.dev/
# Set a blazingly fast keyboard repeat rate
# These numbers are multiples of 15ms
defaults write NSGlobalDomain InitialKeyRepeat -int 20 # before: 25; normal minimum is 15 (225 ms)
defaults write NSGlobalDomain KeyRepeat        -int 5  # before: 6;  normal minimum is 2  (30 ms)

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true
# Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Save to disk (not to iCloud) by default
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false
# Disable automatic capitalization as it’s annoying when typing code
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false
# Disable smart dashes as they’re annoying when typing code
# defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false
# Disable automatic period substitution as it’s annoying when typing code
# defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false
# Disable smart quotes as they’re annoying when typing code
# defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# https://macos-defaults.com/screenshots/location.html
# shellcheck disable=SC2088
defaults write com.apple.screencapture "location" -string "~/Downloads" #&& killall SystemUIServer

# Enable subpixel font rendering on non-Apple LCDs
# Reference: https://github.com/kevinSuttle/macOS-Defaults/issues/17#issuecomment-266633501
# possible values: 0-3
# defaults write NSGlobalDomain AppleFontSmoothing -int 1

# Path for new Finder windows
# https://github.com/mathiasbynens/dotfiles/blob/c886e139233320e29fd882960ba3dd388d57afd7/.macos#L250
# https://apple.stackexchange.com/a/306212
# before (Recents):
# defaults read com.apple.finder NewWindowTarget     => PfAF
# defaults read com.apple.finder NewWindowTargetPath =>  file:///System/Library/CoreServices/Finder.app/Contents/Resources/MyLibraries/myDocuments.cannedSearch
defaults write com.apple.finder NewWindowTarget -string "PfLo"
defaults write com.apple.finder NewWindowTargetPath -string "file://$HOME/Downloads/"

# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Avoid creating .DS_Store files on network or USB volumes
# defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
# defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Automatically open a new Finder window when a volume is mounted
# defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
# defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
# defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

# Show the ~/Library folder
chflags nohidden ~/Library

# https://macos-defaults.com/activity-monitor/updateperiod.html
defaults write com.apple.ActivityMonitor "UpdatePeriod" -int "2" #&& killall Activity\ Monitor
# https://macos-defaults.com/activity-monitor/icontype.html
defaults write com.apple.ActivityMonitor IconType -int 5
# Show all processes in Activity Monitor
defaults write com.apple.ActivityMonitor ShowCategory -int 0

## iTerm 2
# Specify the preferences directory
defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "~/.config/iterm2"
# Tell iTerm2 to use the custom preferences in the directory
defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true

## Hammerspoon
# Put config in ~/.config/hammerspoon instead of ~/.hammerspoon
defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/hammerspoon/init.lua"
