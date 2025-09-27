" Load .vimrc
set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath

set signcolumn=yes " Keep signcolumn on by default
set inccommand=split " Preview substitutions live, as you type!

source ~/.vimrc

lua require("config")
