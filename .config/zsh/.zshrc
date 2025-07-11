# Prompt color settings
autoload -U colors && colors
git_prompt_info() {
    local branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if [ -n "$branch" ]; then
        echo " ($branch)"
    fi
}

PS1='%B%{$fg[green]%}%n@%m:%{$fg[cyan]%}%~$(git_prompt_info)%{$reset_color%}$%b '

# Enable substitution in the prompt.
setopt prompt_subst

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
compdef -d config # Do not enable these completions for my dotfiles function `config` so path completions for dotfiles work

_comp_options+=(globdots)		# Include hidden files.
autoload -U select-word-style
select-word-style bash # So C-w does not delete whole words in things like a path

# History in cache directory:
HISTFILESIZE=1000000
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE="${XDG_CONFIG_HOME:-$HOME/.config}/shell/.history"
# If a new command line being added to the history list duplicates an older one, the older command is removed from the list (even if it is not the previous event).
setopt HIST_IGNORE_ALL_DUPS

bindkey -e
bindkey \^U backward-kill-line

# # Edit line in vim with alt-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^[e' edit-command-line
bindkey -s "^[a" 'configa\n'
bindkey -s '^[p' 'edit-file\n'
bindkey -s '^[o' 'lfcd\n'
bindkey -s '^[m' 'mpcsearch\n'

bdjump(){
    cd $(cat ${XDG_CONFIG_HOME:-$HOME/.config}/shell/all-dirs | fzf --height 15)
}

edit-file () {
file=$(fzf --height 15)
[ -f "$file" ] && $EDITOR "$file"
}


lfcd () {
    # set -e
    tmp="$(mktemp)"
    trap 'rm -f $tmp >/dev/null 2>&1 && trap - HUP INT QUIT TERM PWR EXIT' HUP INT QUIT TERM PWR EXIT
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        lf -last-dir-path="$tmp" "$@"
    else
        [ ! -d "$HOME/.cache/lf" ] && mkdir --parents "$HOME/.cache/lf"
        lf -last-dir-path="$tmp" "$@" 3>&-
    fi
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" >/dev/null
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}

# Plugins
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh 2>/dev/null
