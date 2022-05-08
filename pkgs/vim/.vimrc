"              __
"      __  __ /\_\    ___ ___   _ __   ___
"     /\ \/\ \\/\ \ /' __` __`\/\`'__\/'___\
"   __\ \ \_/ |\ \ \/\ \/\ \/\ \ \ \//\ \__/
"  /\_\\ \___/  \ \_\ \_\ \_\ \_\ \_\\ \____\
"  \/_/ \/__/    \/_/\/_/\/_/\/_/\/_/ \/____/
"
" author: cai <hi@caian.org>
"   code: github.com/caian-org/dots

set nocompatible
set runtimepath+=~/.vim/


" =======
" PLUGINS
" =======

call plug#begin('~/.vim/plugged')

" FEATURES {{{

    Plug 'tpope/vim-repeat'                " Repetition on non-standard commands
    Plug 'itmammoth/doorboy.vim'           " Auto-completion for open-close pair of characters
    Plug 'godlygeek/tabular'               " Text filtering and alignment
    Plug 'google/vim-searchindex'          " Display number of search matches & index of a current match
    Plug 'haya14busa/incsearch.vim'        " Incremental searching
    Plug 'lambdalisue/fern.vim'            " Async tree viewer
    Plug 'antoinemadec/FixCursorHold.nvim' " Neovim performance patch
    Plug 'nacro90/numb.nvim'               " Peek a given line using :<number>
    Plug 'folke/zen-mode.nvim'             " Concentration mode (remove most UI components)
    Plug 'numToStr/FTerm.nvim'             " Floating terminal
    Plug 'triglav/vim-visual-increment'    " Create increasing/decreasing sequence of letters and numbers

" }}}
" APPEARANCE {{{

    Plug 'lilydjwg/colorizer'                     " Colorizes text in #RGB format (#BABACA, #123456, #F0D45E)
    Plug 'itchyny/vim-cursorword'                 " Underlines the word under the cursor
    Plug 'dylanaraps/wal.vim'                     " Pywal's colourscheme in Vim
    Plug 'vim-airline/vim-airline'                " Status bar/tabline
    Plug 'vim-airline/vim-airline-themes'         " Themes for vim-airline
    Plug 'ryanoasis/vim-devicons'                 " Dev icons on tree viewer, tabline, status bar etc
    Plug 'lambdalisue/nerdfont.vim'               " Nerdfont wrapper for fern
    Plug 'lambdalisue/fern-renderer-nerdfont.vim' " Add dev icons on fern tree viewer
    Plug 'lambdalisue/fern-git-status.vim'        " Add git status icons on fern tree viewer

" }}}
" DEV {{{

" Requires: <https://github.com/davidhalter/jedi>
"           <https://github.com/neovim/pynvim>
"           <https://github.com/neovim/node-client>

    Plug 'sheerun/vim-polyglot' " Syntax highlight & filetype plugin pack
    Plug 'mhinz/vim-signify'    " Shows git diff on the gutter
    Plug 'neoclide/coc.nvim', {'branch': 'release'} " Add compatibility with VSCode extensions

" }}}

call plug#end()

" INIT {{{

    lua require('numb').setup()
    lua require('zen-mode').setup()

    command! FTermOpen lua require('FTerm').open()
    command! FTermClose lua require('FTerm').close()

" }}}

" ======================
" PLUGIN CONFIGS / PREFS
" ======================

" VIM-AIRLINE {{{

    let g:airline_theme='base16color'                  " Uses the terminal colourscheme
    let g:airline_powerline_fonts=1                    " Enables powerline symbols
    let g:airline#extensions#tabline#enabled=1         " Shows the tabline
    let g:airline#extensions#tabline#fnamemod=':t'     " Only shows the filename in tabline
    let g:airline#extensions#tabline#left_sep=''       " Removes the 'arrow' glyph on tabline and status bar
    let g:airline#extensions#tabline#left_alt_sep = '' " (idem)
    let g:airline_left_sep=''                          " (idem)
    let g:airline_right_sep=''                         " (idem)

" }}}
" FERN {{{

    " Set CursorHold update time to 100 ms
    let g:cursorhold_updatetime = 100

    " Render fern trees with nerdfont icons
    let g:fern#renderer = 'nerdfont'

    " Function to expand/collapse file trees and enter/leave files
    fun! s:init_fern() abort
        nmap <buffer><expr>
              \ <Plug>(fern-my-expand-or-enter)
              \ fern#smart#drawer(
              \   '\<Plug>(fern-open-or-expand)',
              \   '\<Plug>(fern-open-or-enter)',
              \ )

        nmap <buffer><expr>
              \ <Plug>(fern-my-collapse-or-leave)
              \ fern#smart#drawer(
              \   '\<Plug>(fern-action-collapse)',
              \   '\<Plug>(fern-action-leave)',
              \ )

        nmap <buffer><nowait> l <Plug>(fern-my-expand-or-enter)
        nmap <buffer><nowait> h <Plug>(fern-my-collapse-or-leave)
    endfun

" }}}
" COC {{{

    " Language servers
    let g:coc_global_extensions = [
                \ 'coc-html',
                \ 'coc-css',
                \ 'coc-json',
                \ 'coc-eslint',
                \ 'coc-tsserver',
                \ 'coc-sh',
                \ 'coc-clangd',
                \ 'coc-go',
                \ 'coc-jedi',
                \ 'coc-vimlsp',
                \ 'coc-word',
                \ 'coc-translator',
                \ 'coc-calc',
                \ ]

    " Avoid COC to use the node interpreter set by nvm
    let g:coc_node_path = "/usr/local/bin/node"

" }}}


" ================
" CUSTOM FUNCTIONS
" ================

" ToggleBufferZoomIn {{{

    let g:buffer_zoom_in = 0

    fun ToggleBufferZoomIn()
        if g:buffer_zoom_in
            let g:buffer_zoom_in = 0
            execute 'tabclose'
        else
            let g:buffer_zoom_in = 1
            execute 'tabedit %'
        endif
    endfun

" }}}
" ToggleFloatingTerminal {{{

    let g:floating_term_is_active = 0

    fun ToggleFloatingTerminal()
        if g:floating_term_is_active
            let g:floating_term_is_active = 0
            execute 'FTermClose'
        else
            let g:floating_term_is_active = 1
            execute 'FTermOpen'
        endif
    endfun

" }}}


" ===================
" GENERAL DEFINITIONS
" ===================

" FUNCTIONAL {{{

" Ref: <http://vim.wikia.com/wiki/Using_the_mouse_for_Vim_in_an_xterm>
"      <https://vi.stackexchange.com/questions/2162/why-doesnt-the-backspace-key-work-in-insert-mode>

    let mapleader=' '              " Remaps the 'leader' key
    let maplocalleader=','         " Remaps the 'local leader' key

    set showcmd                    " Shows the entered command & number of selected lines
    set modeline                   " Enable modeline
    set noswapfile                 " Disables swap files
    set undofile                   " Enables persistent undo
    set ignorecase                 " Ignore case in search patterns
    set confirm                    " Use dialog when confirming an operation (such as :q)
    set hidden                     " Hide 'No write since last change' when changing buffers
    set splitbelow                 " Horizontal splits open at the bottom
    set splitright                 " Vertical splits open at the right
    set modelines=5                " Look for the modeline in the first or last 5 lines
    set undodir=~/.vim/undo        " Undo history location (this dir MUST exist)
    set undolevels=2000            " Max number of saved undos
    set undoreload=25000           " Max number of lines to be saved
    set ttimeoutlen=10             " Reduces the delay when changing modes
    set clipboard=unnamed          " Uses the system clipboard
    set mouse=a                    " Mouse scrolling instead of history buffer inside Tmux
    set backspace=indent,eol,start " Make backspace work as expected

" }}}
" VISUAL {{{

" Ref: <http://vim.wikia.com/wiki/Automatic_word_wrapping>

    syntax on                 " Enables syntax highlight
    set number                " Shows line numbers
    set relativenumber        " Uses relative numbering
    set hlsearch              " Highlights search results
    set expandtab             " Expand TABs to spaces
    set wildmenu              " [TAB] as completion key
    set incsearch             " Search incrementally (start matching immediately)
    set noshowmode            " Hide the mode status in the last line
    set tabstop=4             " Set TAB as 4 spaces wide
    set shiftwidth=4          " Shift 4 spaces wide when identing/dedenting
    set scrolloff=0           " Cursor line in the middle of screen
    set fillchars=vert:│      " Makes a continuous line with a special UTF-8 char
    set laststatus=2          " Always display the status line
    set formatoptions+=t      " Automatic word wrapping
    set textwidth=120         " Sets max number of characters width per line
    set wildmode=longest:full " ...
    set wildchar=<Tab>        " ...
    set shortmess+=I          " Don't display the intro message
    set encoding=UTF-8        " Always use UTF-8 character encoding

    colorscheme wal           " Uses pywal's color scheme

    " The following statements must be declared AFTER the color scheme definition in order to work properly.
    hi VertSplit cterm=NONE   " Supress the current color scheme in vertical split line
    hi VertSplit ctermfg=237  " Sets the vertical line foreground color to 237
    hi VertSplit ctermbg=NONE " Nulls the vertical line background color

" }}}


" =========================
" COMMANDS, MAPS AND REMAPS
" =========================

" COMMANDS {{{

    " Typos
    command W w
    command Q q
    command WQ wq

" }}}
" BUFFERS {{{

    " New empty horizontal
    nnoremap <LocalLeader>bh :new<CR>

    " New empty vertical
    nnoremap <LocalLeader>bv :vnew<CR>

    " Close the active buffer
    nnoremap <LocalLeader>bd :BW<CR>

    " List all
    nnoremap <LocalLeader>bl :ls<CR>

    " zoom in and out from the current buffer (similar to tmux's "bind-z")
    nnoremap <LocalLeader>bz :call ToggleBufferZoomIn()<CR>

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
" PLUGINS {{{

    " COC {{{

        " Translate to popup
        nmap <LocalLeader>tp <Plug>(coc-translator-p)

        " Translate to echo
        nmap <LocalLeader>te <Plug>(coc-translator-e)

        " Translate and replace word
        nmap <LocalLeader>tr <Plug>(coc-translator-r)

        " Jump to definition
        nmap <silent> gd <Plug>(coc-definition)

        " Jump to type definition
        nmap <silent> gy <Plug>(coc-type-definition)

        " Jump to implementation
        nmap <silent> gi <Plug>(coc-implementation)

        " Jump to references
        nmap <silent> gr <Plug>(coc-references)

    " }}}

    " Toggle fern tree
    nnoremap <F3> :Fern . -drawer -width=40 -toggle<CR>
    inoremap <F3> <Esc>:Fern . -drawer -width=40 -toggle<CR>a

    " Toggle zen
    nnoremap <F4> :ZenMode<CR>
    inoremap <F4> <Esc>:ZenMode<CR>a

    " Toggle floating terminal
    nnoremap <F5> :call ToggleFloatingTerminal()<CR>
    inoremap <F5> <Esc>:call ToggleFloatingTerminal()<CR>a

    " Disable line numbers on fern buffer
    au FileType fern setlocal nonumber norelativenumber

" }}}
" COPY & PASTE {{{

    " delete without yanking
    nnoremap <LocalLeader>d "_d
    vnoremap <LocalLeader>d "_d

    " replace selected text without yanking
    vnoremap <LocalLeader>p "_dP

" }}}
" MISC {{{

    " Toggles search highlighting
    nnoremap <LocalLeader>s :set hlsearch!<CR>

    " Toggles case-sensitive search
    nnoremap <LocalLeader>c set ignorecase!<CR>

    " Folding with markers ({{{ & }}})
    au FileType vim,zsh,tmux set foldmethod=marker

    " Automatically deletes all trailling whitespace on save
    au BufWritePre * %s/\s\+$//e

    " Disables automatic commenting on newline
    au FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

    " Set the V extension to be related to vlang instead of verilog
    au BufNewFile,BufRead,BufReadPost *.v set ft=vlang

    " Set the indent width to 2 spaces on Nim files
    au BufNewFile,BufRead,BufReadPost *.nim set shiftwidth=2 ft=nim

" }}}
