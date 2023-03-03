PS1='%m %1d $ '
setopt autocd		# Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments

# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"

# Basic auto/tab complete:
autoload -Uz compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# History in cache directory:
export HISTFILESIZE=1000000000
export HISTSIZE=1000000000
export SAVEHIST=10000
export HISTFILE=~/.local/share/history
setopt INC_APPEND_HISTORY
# Don't list history duplications
setopt HIST_FIND_NO_DUPS
# Don't save history duplications to the file
setopt HIST_IGNORE_ALL_DUPS

bindkey -e
bindkey \^U backward-kill-line

# # Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^[e' edit-command-line
bindkey -s "^[a" 'bc -lq\n'
# bindkey -s '^f' 'cd "$(dirname "$(fzf --height 15)")"\n'
bindkey -s '^[p' 'edit-file\n'
bindkey -s '^[o' 'lfcd\n'
bindkey -s '^[m' 'mpcsearch\n'

lfcd () {
   tmp="$(mktemp)"
    trap 'rm -f $tmp >/dev/null 2>&1 && trap - HUP INT QUIT TERM PWR EXIT' HUP INT QUIT TERM PWR EXIT
    lfub -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" >/dev/null
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}

edit-file () {
  file=$(fzf --height 15)
  [ -f "$file" ] && $EDITOR "$file"
}
