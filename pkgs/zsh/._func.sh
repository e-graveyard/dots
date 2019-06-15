# create a dir (with parents) and cd into them
m() {
    mkdir -p -- "$1" &&
        cd -P -- "$1" || return
}

t() {
    if ! tmux ls; then
        tmux -f "$HOME/.config/tmux/.tmux.conf"
    else
        tmux a
    fi
}

# concatenate my Xresources and pywal's generated colorscheme and update
# the database
reload_xrdb() {
    cat "$HOME/.Xresources" "$HOME/.cache/wal/colors.Xresources" | xrdb -
}

# download a mp3 from youtube (w/ the best quality possible)
ytd() {
    youtube-dl \
        --extract-audio \
        --audio-format mp3 \
        --audio-quality 0 \
        "https://www.youtube.com/watch?v=$1"
}

# change wallpaper for all workspaces
chwp() {
    dbus-send \
        --session \
        --dest=org.kde.plasmashell \
        --type=method_call /PlasmaShell org.kde.PlasmaShell.evaluateScript "string:
        var Desktops = desktops();
        for (i=0;i<Desktops.length;i++) {
                d = Desktops[i];
                d.wallpaperPlugin = 'org.kde.image';
                d.currentConfigGroup = Array('Wallpaper', 'org.kde.image', 'General');
                d.writeConfig('Image', 'file://$1');
        }"
}

# set the theme from an image
chtm() {
    wal -n -i "$@"
    chwp "$(cat "$HOME/.cache/wal/wal")"
    reload_xrdb
}

# reload the xresources database before opening emacs
org_mode() {
    reload_xrdb
    nohup emacs --fullscreen & disown
}
