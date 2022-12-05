# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  # source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi
# source ~/.config/zsh/.gruvbox-dark-zsh
# typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet
# PURE_POWER_MODE=modern    # use nerdfont characters in the prompt(default)
autoload -U colors && colors	# Load colors
PS1="%{$fg[blue]%}%{$fg[blue]%}%n%{$fg[black]%}@%{$fg[green]%}%M %{$fg[black]%}%~%{$fg[black]%}%{$reset_color%} > "

#PS1='[%n@%m %~] $ '
# PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
# PS1="%{$fg[red]%}%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}%{$reset_color%}$ "
# PS1="$ "
# PS1="%~%{$fg[red]%}%{$reset_color%} $ "
# PS1="%{$fg[yellow]%}%{$fg[yellow]%}%n%{$fg[yellow]%} (at) %{$fg[yellow]%}%M %{$reset_color%}:%~%{$fg[red]%}%{$reset_color%} $ "
# PS1="%{$fg[yellow]%}%{$fg[yellow]%}%n%{$fg[yellow]%} (at) %{$fg[yellow]%}%M %{$reset_color%}:%~%{$fg[red]%}%{$reset_color%} $ "
# PS1="%{$fg[white]%}%{$fg[white]%}%n%{$fg[white]%}@%{$fg[white]%}%M%{$fg[white]%}:%~%{$fg[white]%}%{$fg[white]%}$ "
# setopt autocd		# Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments




# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
# [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"
#alias config='/usr/bin/git --git-dir=$HOME/dots/ --work-tree=$HOME'
# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# History in cache directory:
export HISTSIZE=10000000
export HISTFILESIZE=100000000
export SAVEHIST=10000
export HISTFILE=~/.config/zsh/.history
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


# emacs mode
bindkey -e
# bindkey -M emacs '^P' history-substring-search-up
# bindkey -M emacs '^N' history-substring-search-down

# vi mode
 # bindkey -v
 # export KEYTIMEOUT=1
 # # Use vim keys in tab complete menu:
 # bindkey -M menuselect 'h' vi-backward-char
 # bindkey -M menuselect 'k' vi-up-line-or-history
 # bindkey -M menuselect 'l' vi-forward-char
 # bindkey -M menuselect 'j' vi-down-line-or-history
 # bindkey -v '^?' backward-delete-char
 # # Edit line in vim with ctrl-e:
 # autoload edit-command-line; zle -N edit-command-line
 # bindkey '^e' edit-command-line
 # # Search history plugin
 # bindkey -M vicmd 'k' history-substring-search-up
 # bindkey -M vicmd 'j' history-substring-search-down
 # bindkey '^[[A' history-substring-search-up
 # bindkey '^[[B' history-substring-search-down

# bindkey -s '^a' 'bc -lq\n'
# bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'
# bindkey '^[[P' delete-char


# # Yank to the system clipboard
# function vi-yank-xclip {
 #  zle vi-yank
 #  echo "$CUTBUFFER" | tr -d '\n'| xclip -i -sel c
# }
# zle -N vi-yank-xclip
# bindkey -M vicmd 'y' vi-yank-xclip

bindkey -s '^o' 'lfcd\n'
# # Use lf to switch directories and bind it to ctrl-o
lfcd () {
   tmp="$(mktemp)"
    lfub -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" >/dev/null
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}


# Change cursor shape for different vi modes.
# function zle-keymap-select () {
#     case $KEYMAP in
#         vicmd) echo -ne '\e[1 q';;      # block
#         viins|main) echo -ne '\e[5 q';; # beam
#     esac
# }
# zle -N zle-keymap-select
# zle-line-init() {
#     zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
#     echo -ne "\e[5 q"
# }
# zle -N zle-line-init
# echo -ne '\e[5 q' # Use beam shape cursor on startup.
# preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.



# Load syntax highlighting; should be last.
# fi
# source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh 2>/dev/null
# source ~/.config/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh 2>/dev/null
# source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh 2>/dev/null
# source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh 2>/dev/null
# source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# # To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
# [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
# [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

alias luamake=/home/mah/personal/sumneko/bin/lua-language-server/3rd/luamake/luamake
