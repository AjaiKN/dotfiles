(require :keymaps)

;; [[ Install `lazy.nvim` plugin manager ]]
;;    See `:help lazy.nvim.txt` or https://github.com/folke/lazy.nvim for more info
(local lazypath (.. (vim.fn.stdpath :data) "/lazy/lazy.nvim"))
(when (not ((. (or vim.uv vim.loop) :fs_stat) lazypath))
  (local lazyrepo "https://github.com/folke/lazy.nvim.git")
  (local out (vim.fn.system [:git :clone "--filter=blob:none" "--branch=stable" lazyrepo lazypath]))
  (when (~= vim.v.shell_error 0)
    (error (.. "Error cloning lazy.nvim:\n" out))))
(vim.opt.rtp:prepend lazypath)

((. (require :lazy) :setup) :plugins)
