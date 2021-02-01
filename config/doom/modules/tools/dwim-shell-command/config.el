;;; tools/dwim-shell-command/config.el -*- lexical-binding: t; -*-

(map! "M-!"                                #'dwim-shell-command
      [remap shell-command]                #'dwim-shell-command
      [remap dired-do-async-shell-command] #'dwim-shell-command
      [remap dired-do-shell-command]       #'dwim-shell-command
      [remap dired-smart-shell-command]    #'dwim-shell-command)
