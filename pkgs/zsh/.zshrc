#                    __
#                   /\ \
#      ____     ____\ \ \___   _ __   ___
#     /\_ ,`\  /',__\\ \  _ `\/\`'__\/'___\
#   __\/_/  /_/\__, `\\ \ \ \ \ \ \//\ \__/
#  /\_\ /\____\/\____/ \ \_\ \_\ \_\\ \____\
#  \/_/ \/____/\/___/   \/_/\/_/\/_/ \/____/
#
# author : cai <hi@caian.org>
#   code : github.com/caian-org/dots


# GENERAL {{{

    # define zsh as hyphen insensitive
    HYPHEN_INSENSITIVE="true"

    # define zsh as case insensitive
    CASE_SENSITIVE="false"

    # enable command correction
    ENABLE_CORRECTION="true"

    # enable colors in ls
    DISABLE_LS_COLORS="false"

    # set neovim as default editor
    export EDITOR="nvim"

    # wow, much colors, so beautiful, 10/10
    export TERM="xterm-256color"

# }}}
# POWERLEVEL9K {{{

    # sets the way pl9k handles the font
    POWERLEVEL9K_MODE="awesome-fontconfig"

    # new line after prompt
    POWERLEVEL9K_PROMPT_ADD_NEWLINE=true
    POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
    POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX=" : "

    # null segment separators
    POWERLEVEL9K_RIGHT_SEGMENT_SEPARATOR=""
    POWERLEVEL9K_RIGHT_SUBSEGMENT_SEPARATOR=""
    POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR=""
    POWERLEVEL9K_LEFT_SUBSEGMENT_SEPARATOR=""

    # "dir_writable" segment
    POWERLEVEL9K_DIR_WRITABLE_FORBIDDEN_BACKGROUND="clear"
    POWERLEVEL9K_DIR_WRITABLE_FORBIDDEN_FOREGROUND="yellow"

    # "dir" segment
    POWERLEVEL9K_DIR_HOME_BACKGROUND="clear"
    POWERLEVEL9K_DIR_HOME_FOREGROUND="blue"
    POWERLEVEL9K_DIR_HOME_SUBFOLDER_BACKGROUND="clear"
    POWERLEVEL9K_DIR_HOME_SUBFOLDER_FOREGROUND="blue"
    POWERLEVEL9K_DIR_DEFAULT_BACKGROUND="clear"
    POWERLEVEL9K_DIR_DEFAULT_FOREGROUND="white"

    # "command_execution_time" segment
    POWERLEVEL9K_COMMAND_EXECUTION_TIME_BACKGROUND="clear"
    POWERLEVEL9K_COMMAND_EXECUTION_TIME_FOREGROUND="green"

    # "vcs" segment (git)
    POWERLEVEL9K_VCS_CLEAN_FOREGROUND="green"
    POWERLEVEL9K_VCS_CLEAN_BACKGROUND="clear"
    POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND="magenta"
    POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND="clear"
    POWERLEVEL9K_VCS_MODIFIED_FOREGROUND="red"
    POWERLEVEL9K_VCS_MODIFIED_BACKGROUND="clear"

    # "status" segment
    POWERLEVEL9K_STATUS_OK_BACKGROUND="clear"
    POWERLEVEL9K_STATUS_OK_FOREGROUND="green"
    POWERLEVEL9K_STATUS_ERROR_BACKGROUND="clear"
    POWERLEVEL9K_STATUS_ERROR_FOREGROUND="red"

    # print execution_time if threshold >= 3
    POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=2

    # shorten path
    POWERLEVEL9K_SHORTEN_DIR_LENGTH=3
    POWERLEVEL9K_SHORTEN_STRATEGY="truncate_middle"

    # terminal prompt in new line
    POWERLEVEL9K_PROMPT_ON_NEWLINE=true

    # left elements/segments
    POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(status dir_writable dir)

    # right elements/segments
    POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(command_execution_time vcs)

# }}}
# OH-MY-ZSH {{{

    export ZSH="${HOME}/.oh-my-zsh"

    # update frequency
    export UPDATE_ZSH_DAYS=7

    # theme definition
    ZSH_THEME="powerlevel10k/powerlevel10k"

    # plugin definition
    plugins=(
        git
        encode64
        vi-mode
        urltools
        tmux
        python
        pip
        zsh-autosuggestions
        zsh-syntax-highlighting
        )

    # source oh-my-zsh
    source "${ZSH}/oh-my-zsh.sh"

# }}}
# SOURCES {{{

    # environment variables
    source "$HOME/._env.sh"

    # locations in PATH
    source "$HOME/._path.sh"

    # aliases
    source "$HOME/._alias.sh"

# }}}
