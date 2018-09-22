fetch_tmux_plugins()
{
    local plugins=(
        'tmux-prefix-highlight'
        'tmux-resurrect'
        'tmux-yank'
    )

    if [ -x "$(command -v git)" ]; then
        for plugin in "${plugins[@]}"; do
            if ! [ -d "${HOME}/.config/tmux/${plugin}" ]; then
                echo "Fetching ${plugin}..."
                git clone "https://github.com/tmux-plugins/${plugin}.git" "$HOME/.config/tmux/${plugin}" > /dev/null 2>&1
            fi
        done
    fi
}
