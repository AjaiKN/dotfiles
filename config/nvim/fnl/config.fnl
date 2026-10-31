(require :keymaps)

;; Don't load or install plugins as root: $HOME might still be my user's (macOS's
;; sudo keeps it), so they'd end up owned by root. See is_root in vimrc.
(when (~= ((. (or vim.uv vim.loop) :getuid)) 0)
  ;; [[ Install `lazy.nvim` plugin manager ]]
  ;;    See `:help lazy.nvim.txt` or https://github.com/folke/lazy.nvim for more info
  (local lazypath (.. (vim.fn.stdpath :data) "/lazy/lazy.nvim"))
  (when (not ((. (or vim.uv vim.loop) :fs_stat) lazypath))
    (local lazyrepo "https://github.com/folke/lazy.nvim.git")
    (local out (vim.fn.system [:git :clone "--filter=blob:none" "--branch=stable" lazyrepo lazypath]))
    (when (~= vim.v.shell_error 0)
      (error (.. "Error cloning lazy.nvim:\n" out))))
  (vim.opt.rtp:prepend lazypath)

  ((. (require :lazy) :setup) :plugins))
