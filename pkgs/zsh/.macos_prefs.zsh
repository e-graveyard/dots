#       ___ ___      __      ___    ___     ____
#     /' __` __`\  /'__`\   /'___\ / __`\  /',__\
#   __/\ \/\ \/\ \/\ \L\.\_/\ \__//\ \L\ \/\__, `\
#  /\_\ \_\ \_\ \_\ \__/.\_\ \____\ \____/\/\____/
#  \/_/\/_/\/_/\/_/\/__/\/_/\/____/\/___/  \/___/
#
# author : cai <hi@caian.org>
#   code : github.com/caian-org/dots


# ENVIRONMENT {{{

    # PROJ_DIR
    export PROJ_DIR="$HOME/Projs"

    export LC_ALL="en_GB.UTF-8"

# }}}
# PATH {{{

    NIMBLE_BIN="$HOME/.nimble/bin"
    PYTHON_BIN="$HOME/Library/Python/3.9/bin"
    export PATH="$PATH:/usr/local/sbin:$PYTHON_BIN:$NIMBLE_BIN"

# }}}
# ALIASES {{{

    # take content from STDOUT to clipboard
    # usage, e.g.: pwgen -cns 32 1 | to_clipboard
    alias to_clipboard="pbcopy"

# }}}
# FUNCTIONS {{{

    # change wallpaper for current workspace
    chwp() {
        osascript -e "tell application \"Finder\" to set desktop picture to POSIX file \"$1\""
    }

    # set the theme from an image
    chtm() {
        wal -n -i "$@"
        chwp "$(cat "$HOME/.cache/wal/wal")"
    }

    # nvm severely slows down the shell startup; let's load nvm only when necessary
    init_nvm() {
        # loads nvm
        [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

        # loads shell completions
        [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
    }

    init_rvm() {
        [ -s "$RVM_DIR/scripts/rvm" ] && \. "$RVM_DIR/scripts/rvm"
    }

# }}}
