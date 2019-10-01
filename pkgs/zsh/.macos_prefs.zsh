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

    PYTHON_BIN="$HOME/Library/Python/3.7/bin"
    export PATH="$PATH:/usr/local/sbin:$PYTHON_BIN"

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

# }}}
