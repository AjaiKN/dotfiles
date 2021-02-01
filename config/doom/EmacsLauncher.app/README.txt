This app opens a zsh shell, uses Emacs.app and hides it.

Why do this?

1. emacs-mac (https://bitbucket.org/mituharu/emacs-mac/src/master/) doesn't support
   having a separate daemon (https://github.com/railwaycat/homebrew-emacsmacport/issues/52).
   (https://ylluminarious.github.io/2019/05/23/how-to-fix-the-emacs-mac-port-for-multi-tty-access/)
2. The main reason: if I launch Emacs.app normally, then I connect to it with dotfiles/bin/emacs-term,
   the colors are weird. If I launch it from a zsh shell, then it works fine when connecting to it.
   NOTE: Interestingly, if I launch it normally but then run `M-x doom/restart` (SPC q R),
   the colors *do* work when connecting to it from `emacs-term`.

You can edit it by editing
EmacsLauncher.app/EmacsLauncher
(See https://apple.stackexchange.com/a/269045 )
