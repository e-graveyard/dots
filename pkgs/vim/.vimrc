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
" SPAWNER {{{

    let sources = [
                \ 'plugs',
                \ 'general',
                \ 'commands'
                \]

    for source in sources
        let filename = expand(source . '.vim')
        let filepath = expand($HOME . '/.vim/' . filename)
        let remote = expand('https://raw.githubusercontent.com/caian-org/dots/master/pkgs/vim/.vim/' . filename)

        if !filereadable(filepath) || empty(glob(filepath))
            execute 'silent !curl -fLo ' . filepath . ' ' . remote
            execute 'redraw'
        endif

        execute 'source' filepath
    endfor

" }}}
