"              __
"      __  __ /\_\    ___ ___   _ __   ___
"     /\ \/\ \\/\ \ /' __` __`\/\`'__\/'___\
"   __\ \ \_/ |\ \ \/\ \/\ \/\ \ \ \//\ \__/
"  /\_\\ \___/  \ \_\ \_\ \_\ \_\ \_\\ \____\
"  \/_/ \/__/    \/_/\/_/\/_/\/_/\/_/ \/____/
"
" author: cai <hi@caian.org>
"   code: github.com/caian-org/dots


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

    Plug 'majutsushi/tagbar'          " Class outline viewer
    Plug 'sheerun/vim-polyglot'       " Language collection pack
    Plug 'fatih/vim-go'               " IDE-like tools for Golang
    Plug 'Shougo/neosnippet.vim'      " Snippet support
    Plug 'Shougo/neosnippet-snippets' " Snippet source

" }}}
" VCS {{{

    Plug 'airblade/vim-gitgutter'      " Shows git diff in the gutter
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
