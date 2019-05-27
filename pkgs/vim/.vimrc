"              __
"      __  __ /\_\    ___ ___   _ __   ___
"     /\ \/\ \\/\ \ /' __` __`\/\`'__\/'___\
"   __\ \ \_/ |\ \ \/\ \/\ \/\ \ \ \//\ \__/
"  /\_\\ \___/  \ \_\ \_\ \_\ \_\ \_\\ \____\
"  \/_/ \/__/    \/_/\/_/\/_/\/_/\/_/ \/____/
"
" author: cai <hi@caian.org>
"   code: github.com/caian-org/dots


" ===========
" INITIALIZER
" ===========

set nocompatible

" DIRECTORY {{{

    if !isdirectory($HOME . "/.vim")
        call mkdir($HOME . "/.vim", "", 0700)
    endif


    if !isdirectory($HOME . "/.vim/undo")
        call mkdir($HOME . "/.vim/undo", "", 0700)
    endif

    set runtimepath+=~/.vim/

" }}}


" =================
" PLUGIN MANAGEMENT
" =================

" PLUG AUTOINSTALL {{{

    if empty(glob('~/.vim/autoload/plug.vim'))
        silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
            \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    endif

" }}}

call plug#begin('~/.vim/plugged')

" NEOVIM {{{

" Requires: <https://github.com/davidhalter/jedi>
"           <https://github.com/neovim/pynvim>
"           <https://github.com/mdempsky/gocode>

    if has('nvim')
        Plug 'w0rp/ale' " Async Lint Engine

        " Async keyword completion system
        Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

        " Deoplete source...
        Plug 'Shougo/neco-vim'          " ...for VimL
        Plug 'Shougo/deoplete-clangx'   " ...for C/C++
        Plug 'zchee/deoplete-jedi'      " ...for Python
        Plug 'carlitux/deoplete-ternjs' " ...for JavaScript
        Plug 'zchee/deoplete-go', { 'do': 'make' } " ...for Go

        Plug 'Shougo/denite.nvim' " Fuzzy finder + stuff
    endif

"}}}
" UI {{{

    Plug 'lilydjwg/colorizer'      " Colorizes text in #RGB format (#BABACA, #123456, #F0D45E)
    Plug 'itchyny/vim-cursorword'  " Underlines the word under the cursor
    Plug 'dylanaraps/wal.vim'      " Pywal's colourscheme in Vim
    Plug 'junegunn/limelight.vim'  " Hyperfocus-writing
    Plug 'junegunn/goyo.vim'       " Distraction-free writing
    Plug 'vim-airline/vim-airline' " Status bar/tabline
    Plug 'vim-airline/vim-airline-themes' " Themes for vim-airline

" }}}
" UTILS {{{

    Plug 'tpope/vim-surround'       " Quoting/parenthesizing
    Plug 'itmammoth/doorboy.vim'    " Auto-completion for open-close pair of characters
    Plug 'godlygeek/tabular'        " Text filtering and alignment
    Plug 'google/vim-searchindex'   " Display number of search matches & index of a current match
    Plug 'haya14busa/incsearch.vim' " Incremental searching
    Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' } " Filesystem explorer

" }}}
" DEV {{{

" Requires: <https://github.com/derekparker/delve>
"           <https://github.com/universal-ctags/ctags>

    Plug 'jreybert/vimagit'             " ...
    Plug 'ludovicchabant/vim-gutentags' " ...
    Plug 'majutsushi/tagbar'            " Class outline viewer
    Plug 'sheerun/vim-polyglot'         " Language collection pack
    Plug 'fatih/vim-go'                 " IDE-like tools for Golang
    Plug 'Shougo/neosnippet.vim'        " Snippet support
    Plug 'Shougo/neosnippet-snippets'   " Snippet source
    Plug 'davidhalter/jedi-vim'         " ...

    " ...
    Plug 'ternjs/tern_for_vim', { 'do': 'npm i' }

" }}}
" VCS {{{

    Plug 'mhinz/vim-signify'           " Shows git diff in the gutter
    Plug 'Xuyuanp/nerdtree-git-plugin' " Git status flags in NERDTree

" }}}
" MARKUP {{{

    Plug 'plasticboy/vim-markdown' " Syntax highlight and other stuff

" }}}
" WEB {{{

    Plug 'elzr/vim-json'                " A better JSON for Vim
    Plug 'othree/html5.vim'             " Omnicomplete for HTML5
    Plug 'hail2u/vim-css3-syntax'       " CSS syntax support
    Plug 'nikvdp/ejs-syntax'            " EJS syntax support

" }}}
" SYNTAX {{{

    Plug 'mboughaba/i3config.vim' " syntax highlight for i3 config file

" }}}

call plug#end()

" Triggers deoplete (auto-completion)
if has('nvim')
    call deoplete#enable()
endif


" ==================
" PLUGIN PREFERENCES
" ==================

" ALE {{{

    let g:ale_linters = {
    \    'python': ['bandit', 'prospector', 'pycodestyle', 'pydocstyle'],
    \    'javascript': ['eslint'],
    \}

    let g:ale_sign_column_always = 1
    let g:ale_lint_on_text_changed = 'never'
    let b:ale_warn_about_trailing_whitespace = 0

" }}}
" VIM-AIRLINE {{{

    let g:airline_theme='base16color'                   " Uses the terminal colourscheme
    let g:airline_powerline_fonts=1                     " Enables powerline symbols
    let g:airline#extensions#tabline#enabled=1          " Shows the tabline
    let g:airline#extensions#tabline#fnamemod=':t'      " Only shows the filename in tabline
    let g:airline#extensions#tabline#left_sep=' '       " Removes the `arrow` glyph on tab
    let g:airline#extensions#tabline#left_alt_sep = '|' " Removes the `arrow` glyph on the separator

" }}}
" VIM-MARKDOWN {{{

    let g:vim_markdown_folding_disabled=0 " Disable folding
    let vim_markdown_preview_github=1     " Preview with GitHub-flavoured markdown

" }}}
" NERDTREE {{{

    " Ignores Python's byte-compiled files in NERDTree
    let NERDTreeIgnore=['\.pyc$', '\.pyo$', '__pycache__$']

    " Git indicators
    let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "!",
    \ "Staged"    : "+",
    \ "Untracked" : "?",
    \ "Dirty"     : "x",
    \ }

" }}}
" DEOPLETE {{{

    "Add extra filetypes
    let g:deoplete#sources#ternjs#filetypes = [
                    \ 'jsx',
                    \ 'javascript.jsx',
                    \ ]

" }}}


" ================
" CUSTOM FUNCTIONS
" ================

" ToggleCursorInTheMiddle {{{

    let g:cursor_in_the_middle = 1

    fun ToggleCursorInTheMiddle()
        if g:cursor_in_the_middle
            set scrolloff=0
            let g:cursor_in_the_middle = 0
        else
            set scrolloff=999
            let g:cursor_in_the_middle = 1
        endif
    endfun

" }}}


" ===================
" GENERAL DEFINITIONS
" ===================

" FUNCTIONAL {{{

" Ref: <http://vim.wikia.com/wiki/Using_the_mouse_for_Vim_in_an_xterm>
"      <https://vi.stackexchange.com/questions/2162/why-doesnt-the-backspace-key-work-in-insert-mode>

    let mapleader='\'       " Remaps the 'leader' key
    let maplocalleader=','  " Remaps the 'local leader' key

    set showcmd             " Shows the entered command & number of selected lines
    set noswapfile          " Disables swap files
    set undofile            " Enables persistent undo
    set undodir=~/.vim/undo " Undo history location (this dir MUST exist)
    set undolevels=2000     " Max number of saved undos
    set undoreload=25000    " Max number of lines to be saved
    set ttimeoutlen=10      " Reduces the delay when changing modes
    set clipboard=unnamed   " Uses the system clipboard
    set mouse=a             " Mouse scrolling instead of history buffer inside Tmux
    set ignorecase          " Ignore case in search patterns
    set confirm             " Use dialog when confirming an operation (such as :q)
    set hidden              " Hide 'No write since last change' when changing buffers
    set splitbelow          " Horizontal splits open at the bottom
    set splitright          " Vertical splits open at the right
    set backspace=indent,eol,start " Make backspace work as expected

    " When outside of neovim...
    if !has("nvim")
        set ttyfast    " More characters sent to the screen for redrawing
        set lazyredraw " Don't redraw the screen while executing macros
    endif

" }}}
" VISUAL {{{

" Ref: <http://vim.wikia.com/wiki/Automatic_word_wrapping>

    syntax on                 " Enables syntax highlight
    set number                " Shows line numbers
    set relativenumber        " Uses relative numbering
    set hlsearch              " Highlights search results
    set expandtab             " Spaces as [TAB]s
    set tabstop=4             " Set TAB as 4 spaces wide
    set shiftwidth=4          " Shift 4 spaces wide in reindent operations
    set scrolloff=999         " Cursor line in the middle of screen
    set fillchars=vert:│      " Makes a continuous line with a special UTF-8 char
    set laststatus=2          " Always display the status line
    set formatoptions+=t      " Automatic word wrapping
    set textwidth=79          " Sets max number of characters width per line
    set wildmenu              " [TAB] as completion key
    set wildmode=longest:full " ...
    set wildchar=<Tab>        " ...
    set shortmess+=I          " Don't display the intro message
    set incsearch             " Search incrementally (start matching immediately)
    set noshowmode            " Hide the mode status in the last line

    if has('multi_byte')
        set encoding=utf-8    " Always use UTF-8 character encoding
    endif

    colorscheme wal           " Uses pywal's colour scheme

    " The following statements must be declared BELOW the color scheme
    " definition in order to work properly.
    hi VertSplit cterm=NONE   " Supress the current colour scheme in vertical split line
    hi VertSplit ctermfg=237  " Sets the vertical line foreground color to 237
    hi VertSplit ctermbg=NONE " Nulls the vertical line background color

" }}}


" =================
" COMMANDS & REMAPS
" =================

" TERMINAL {{{

    " Open terminal
    nnoremap <LocalLeader>t :terminal<CR>

    " Escape to normal mode inside a terminal buffer more easily
    tnoremap <Esc> <C-\><C-n>

" }}}
" NAVIGATION {{{

    " Up and down by "line on screen" rather than "physical line"
    nmap <C-j> gj
    nmap <C-k> gk

    imap <C-j> <C-o>gj
    imap <C-k> <C-o>gk

    " Cut a line in VISUAL LINE without entering the INSERT MODE (works similar to <d>elete)
    xnoremap m c<BS><Esc>

    " Copy selected text to system clipboard (requires gvim)
    vnoremap <C-c> "+y

" }}}
" BUFFERS {{{

    " New empty horizontal
    nnoremap <LocalLeader>bh :new<CR>

    " New empty vertical
    nnoremap <LocalLeader>bv :vnew<CR>

    " Close the active buffer
    nnoremap <LocalLeader>bd :bd<CR>

    " List all
    nnoremap <LocalLeader>bl :ls<CR>

    " Go to previous
    nnoremap <C-o> :bp<CR>
    inoremap <C-o> <Esc>:bp<CR>

    " Go to next
    nnoremap <C-p> :bn<CR>
    inoremap <C-p> <Esc>:bn<CR>

    " Decrease vertical split height
    nnoremap <C-h> :vertical resize -1<CR>
    inoremap <C-h> <ESC>:vertical resize -1<CR>a

    " Increase vertical split width
    nnoremap <C-l> :vertical resize +1<CR>
    inoremap <C-l> <ESC>:vertical resize +1<CR>a

    " Decrease horizontal split height
    nnoremap <C-j> :resize +1<CR>
    inoremap <C-j> <ESC>:resize +1<CR>a

    " Increase horizontal split width
    nnoremap <C-k> :resize -1<CR>
    inoremap <C-k> <ESC>:resize -1<CR>a

" }}}
" WINDOWS {{{

    " Move around windows more easily
    nnoremap <C-Up> :wincmd k<CR>
    inoremap <C-Up> <C-o>:wincmd k<CR>

    nnoremap <C-Down> :wincmd j<CR>
    inoremap <C-Down> <C-o>:wincmd j<CR>

    nnoremap <C-Left> :wincmd h<CR>
    inoremap <C-Left> <C-o>:wincmd h<CR>

    nnoremap <C-Right> :wincmd l<CR>
    inoremap <C-Right> <C-o>:wincmd l<CR>

" }}}
" ALIASES {{{

    " Prevents Vim to yell at me when I use capitalized commands
    command W w
    command Q q
    command WQ wq

" }}}
" PLUGINS {{{

    " Open NERDTree
    nnoremap <F3> :NERDTreeToggle<CR>
    inoremap <F3> <Esc>:NERDTreeToggle<CR>a

    " Open Tagbar
    nnoremap <F4> :TagbarToggle<CR>
    inoremap <F4> <Esc>:TagbarToggle<CR>a

    " Open Tagbar
    nnoremap <F9> :Goyo 90<CR>
    inoremap <F9> <Esc>:Goyo 90<CR>a

    " ...
    autocmd FileType javascript.jsx nnoremap <LocalLeader>jj :TernDef<CR>
    autocmd FileType javascript.jsx nnoremap <LocalLeader>jd :TernDoc<CR>
    autocmd FileType javascript.jsx nnoremap <LocalLeader>jt :TernType<CR>
    autocmd FileType javascript.jsx nnoremap <LocalLeader>jr :TernRename<CR>

" }}}
" FOLDING {{{

    " Folding with markers ({{{ & }}})
    autocmd FileType vim set foldmethod=marker
    autocmd FileType zsh set foldmethod=marker
    autocmd FileType tmux set foldmethod=marker

    " Toggles folding with <Space>
    autocmd FileType vim nnoremap <Space> za
    autocmd FileType zsh nnoremap <Space> za
    autocmd FileType tmux nnoremap <Space> za

" }}}
" MISC {{{

    " Toggle between cursor at the middle or "normal"
    nnoremap <LocalLeader>c :call ToggleCursorInTheMiddle()<CR>

    " Toggles search highlighting
    nnoremap <LocalLeader>s :set hlsearch!<CR>

    " Automatically deletes all tralling whitespace on save
    autocmd BufWritePre * %s/\s\+$//e

    " Disables automatic commenting on newline
    autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" }}}
