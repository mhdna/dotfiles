# Prompt
autoload -U colors zsh/terminfo
colors

# Find and set branch name var if in git repository.
function git_branch_name()
{
  branch=$(git symbolic-ref HEAD 2> /dev/null | awk 'BEGIN{FS="/"} {print $NF}')
  if [[ $branch == "" ]];
  then
    :
  else
    echo '- ('$branch')'
  fi
}

# Enable substitution in the prompt.
setopt prompt_subst

# Config for prompt. PS1 synonym.
prompt='%F{green}%2/ $(git_branch_name) > '

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
HISTFILESIZE=1000000
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/shell/history"
# If a new command line being added to the history list duplicates an older one, the older command is removed from the list (even if it is not the previous event).
setopt HIST_IGNORE_ALL_DUPS

bindkey -e
bindkey \^U backward-kill-line

# # Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^[e' edit-command-line
bindkey -s "^[a" 'configa\n'
bindkey -s "^[c" 'bc -lq\n'
bindkey -s '^[p' 'edit-file\n'
bindkey -s '^[o' 'lfcd\n'
bindkey -s '^[j' 'bdjump\n'
bindkey -s '^[m' 'mpcsearch\n'

bdjump(){
    cd $(cat ${XDG_CONFIG_HOME:-$HOME/.config}/shell/all-dirs | fzf --height 15)
}


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

# enable plugins
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
