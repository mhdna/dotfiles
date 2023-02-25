PS1='\[\e[0;36m\][\[\e[0;32m\]\u\[\e[0;36m\]@\[\e[0;32m\]\H\[\e[0;36m\]]\[\e[0;36m\][\[\e[0;34m\]\w\[\e[0;36m\]]\[\e[0;32m\]\$ \[\e[0m\]'

[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"

# Eternal bash history.
export HISTFILESIZE=
export HISTSIZE=
export HISTFILE=~/.cache/bash/history

bind '"\ea":"bc -lq\n"'
bind '"\ep":"fzf-prompt\n"'
bind '"\eo":"lfcd\n"'
bind '"\em":"mpcsearch\n"'

fzf-prompt () {
  file=$(fzf --height 15)
  [ -f "$file" ] && $EDITOR "$file"
}

lfcd () {
   tmp="$(mktemp)"
    # trap 'rm -f $tmp >/dev/null 2>&1 && trap - HUP INT QUIT TERM PWR EXIT' HUP INT QUIT TERM PWR EXIT
    lfub -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" >/dev/null
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
