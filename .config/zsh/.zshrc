# Prompt
autoload -U colors zsh/terminfo
colors

setprompt() {
  setopt prompt_subst

  if [[ -n "$SSH_CLIENT"  ||  -n "$SSH2_CLIENT" ]]; then
    p_host='%F{yellow}%M%f'
  else
    p_host='%F{green}%M%f'
  fi

  PS1=${(j::Q)${(Z:Cn:):-$'
    %F{cyan}[%f
    %(!.%F{red}%n%f.%F{green}%n%f)
    %F{cyan}@%f
    ${p_host}
    %F{cyan}][%f
    %F{blue}%~%f
    %F{cyan}]%f
    ${vim_mode}
    %(!.%F{red}%#%f.%F{green}%#%f)
    " "
  '}}

  PS2=$'%_>'
  RPROMPT=$'${vcs_info_msg_0_}'
}
setprompt


setopt autocd		# Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments

# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# Basic auto/tab complete:
autoload -Uz compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# History in cache directory:
export HISTSIZE=10000
export HISTFILESIZE=10000
export SAVEHIST=10000
export HISTFILE=~/.cache/zsh/history
setopt INC_APPEND_HISTORY
# export HISTTIMEFORMAT="[%F %T] "
# Add timestamp to history (it's excution time) -showed with -E flag-
# setopt EXTENDED_HISTORY
# Don't list history duplications, but save them to the file
setopt HIST_FIND_NO_DUPS
# Don't save history duplications
setopt HIST_IGNORE_ALL_DUPS
setopt   HIST_IGNORE_SPACE
setopt   HIST_IGNORE_DUPS

# vi mode
bindkey -v
export KEYTIMEOUT=1
# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char
bindkey -s '^r' '$(tac ~/.cache/zsh/history | fzf --height 15)\n'
# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line
bindkey -s '^a' 'bc -lq\n'
# bindkey -s '^f' 'cd "$(dirname "$(fzf --height 15)")"\n'
bindkey -s '^f' 'edit-file\n'
bindkey -s '^o' 'lfcd\n'
bindkey '^[[P' delete-char


# # # Yank to the system clipboard
function vi-yank-xclip {
  zle vi-yank
  echo "$CUTBUFFER" | tr -d '\n'| xclip -i -sel c
}
zle -N vi-yank-xclip
bindkey -M vicmd 'y' vi-yank-xclip

edit-file () {
  file=$(fzf --height 15)
  [ -f "$file" ] && $EDITOR "$file"
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

# vi normal mode indicator
vim_cmd_mode="%{$fg[blue]%}NOR%{$reset_color%}"
function zle-keymap-select {
  vim_mode="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)}"
  zle reset-prompt
}
zle -N zle-keymap-select
