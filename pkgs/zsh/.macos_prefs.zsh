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

# }}}
# PATH {{{

    export PATH="$PATH:/usr/local/sbin"

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
