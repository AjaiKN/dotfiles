set nocompatible
set mouse=a
syntax enable
set number
" set softtabstop=2
" set ts=2 sw=2
set clipboard+=unnamedplus
set ignorecase
set smartcase
set breakindent
" set signcolumn=yes " Keep signcolumn on by default
set updatetime=250
set timeoutlen=300
set splitright
set splitbelow
set cursorline " Show which line your cursor is on
" set scrolloff=10
set confirm

let data_dir = has('nvim') ? stdpath('data') . '/site' : $HOME . '/.vim'
let plug_dir = data_dir . '/plugged'
let undo_dir = data_dir . '/undo-dir'
let &undodir = undo_dir

" https://vi.stackexchange.com/a/53
" Saving undo info
if !isdirectory(data_dir)
    call mkdir(data_dir, 'p', 0700)
endif
if !isdirectory(plug_dir)
    call mkdir(plug_dir, 'p', 0700)
endif
if !isdirectory(undo_dir)
    call mkdir(undo_dir, 'p', 0700)
endif
set undofile

" Clear search highlighting with Esc
if empty($SSH_CLIENT) && empty($SSH_TTY)
    nnoremap <Esc> :nohlsearch<CR>
else
    " Disabled over SSH to avoid startup issues; otherwise vim starts in insert mode.
    " Over SSH, press Esc twice to clear search highlighting.
    nnoremap <Esc><Esc> :nohlsearch<CR>
endif

" Sets how neovim will display certain whitespace characters in the editor.
"  See `:help 'list'`
"  and `:help 'listchars'`
set list
set listchars=tab:»\ ,nbsp:␣
" trail:·,
" match ErrorMsg '\s\+$'

:hi Normal guibg=NONE ctermbg=NONE
" :hi EndOfBuffer guibg=NONE ctermbg=NONE
" :hi StatusLine guibg=NONE ctermbg=NONE

command ReloadVimrc :source ~/.vimrc

" File Explorer (https://shapeshed.com/vim-netrw/)
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 20
"if !exists('g:vscode')
"  augroup ProjectDrawer
"    autocmd!
"    autocmd VimEnter * :Vexplore
"
"    " https://stackoverflow.com/questions/40729525/open-default-file-in-vim-if-no-argument-given
"    " https://vi.stackexchange.com/questions/10360/define-active-window-for-startup-in-vimrc
"    autocmd VimEnter * wincmd l
"    autocmd VimEnter * if eval("@%") == "" | wincmd h | endif
"  augroup END
"endif

nnoremap <SPACE> <Nop>
let mapleader = ' '
let maplocalleader = ','

" D- = command/super
noremap <D-s> <cmd>w<cr>
inoremap <D-s> <cmd>w<cr>
noremap <D-Left> ^
inoremap <D-Left> <C-o>^
noremap <D-Right> $
inoremap <D-Right> <C-o>$
noremap <M-Left> b
inoremap <M-Left> <C-o>b
noremap <M-Right> e
inoremap <M-Right> <C-o>e<Right>

set keymodel=startsel

" https://stackoverflow.com/questions/6488683/how-to-change-the-cursor-between-normal-and-insert-modes-in-vim
if !has('nvim')
	let &t_SI = "\e[6 q"
	let &t_EI = "\e[2 q"
	set ttimeout
	if !empty($SSH_CLIENT) || !empty($SSH_TTY)
		set ttimeoutlen=100
	else
		set ttimeoutlen=1
	endif
	set ttyfast
endif

if !has('nvim')
	" https://github.com/junegunn/vim-plug/wiki/tips#conditional-activation
	function! Cond(cond, ...)
		let opts = get(a:000, 0, {})
		return a:cond ? opts : extend(opts, { 'on': [], 'for': [] })
	endfunction

	if empty(glob(data_dir . '/autoload/plug.vim'))
		silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
		autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
	endif

	" vim-plug
	call plug#begin(plug_dir)
		Plug 'tpope/vim-commentary'
		Plug 'tpope/vim-sleuth'
		Plug 'easymotion/vim-easymotion'
		Plug 'tommcdo/vim-lion'
		Plug 'justinmk/vim-sneak'
		Plug 'tpope/vim-surround'
		Plug 'tpope/vim-unimpaired'
		Plug 'liuchengxu/vim-which-key'
		Plug 'tpope/vim-fugitive'
		Plug 'ctrlpvim/ctrlp.vim', Cond(!exists('g:vscode'))
		Plug 'rhysd/git-messenger.vim'
		Plug 'psliwka/vim-smoothie'
		"Plug 'preservim/nerdtree'
	call plug#end()
endif
