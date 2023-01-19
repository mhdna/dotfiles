#!/bin/zsh

# If you don't plan on reverting to bash, you can remove the link in ~/.profile
# to clean up.

#functions path
fpath+=("$ZDOTDIR/functions")
export JAVA_HOME=/usr/lib/jvm/default
export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export PATH="$PATH:${$(find ~/.local/bin -type d -printf %p:)%%:}":~/.npm-global/bin:$GOPATH/bin:~/.local/share/cargo/bin:$JAVA_HOME/bin

unsetopt PROMPT_SP

# export GPG_TTY=$(tty)

# Default programs:
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="firefox"
export VIDEO="mpv"
export IMAGE="sxiv"
export CM_SELECTIONS="clipboard"
#export QT_STYLE_OVERRIDE=kvantum #gtk2
# export QT_STYLE_OVERRIDE=adwaita
export CM_DEBUG=1
export CM_OUTPUT_CLIP=1
export CM_MAX_CLIPS=50

# Enable wayland for firefox and others
# MOZZ_ENABLE_WAYLAND=1

export XDG_DOWNLOAD_DIR="$HOME/dl"
export XDG_MUSIC_DIR="$HOME/aud"
export XDG_VIDEOS_DIR="$HOME/vids"
export XDG_PICTURES_DIR="$HOME/pix"
export XDG_DOCUMENTS_DIR="$HOME/dox"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XRESOURCES="${XDG_CONFIG_HOME:-$HOME/.config}/x11/xresources"
export XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc"
export XSESSION="${XDG_CONFIG_HOME:-$HOME/.config}/x11/xsession"
# export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" # This line will break some DMs.
export NOTMUCH_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/notmuch-config"
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc-2.0"
export LESSHISTFILE="-"
export WGETRC="${XDG_CONFIG_HOME:-$HOME/.config}/wget/wgetrc"
# export INPUTRC="${XDG_CONFIG_HOME:-$HOME/.config}/shell/inputrc"
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"
# Might break pipewire (If it doesn't exist) --mah
# export ALSA_CONFIG_PATH="$XDG_CONFIG_HOME/alsa/asoundrc"
# export GNUPGHOME="${XDG_CONFIG_HOME:-$HOME/.config}/gnupg"
export WINEPREFIX="${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes/default"
export KODI_DATA="${XDG_DATA_HOME:-$HOME/.local/share}/kodi"
export PASSWORD_STORE_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/password-store"
export TMUX_TMPDIR="$XDG_RUNTIME_DIR"
export ANDROID_SDK_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/android"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"
export ANSIBLE_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/ansible/ansible.cfg"
export UNISON="${XDG_DATA_HOME:-$HOME/.local/share}/unison"
export HISTFILE="${XDG_CONFIG_HOME:-$HOME/.config}/zsh/history"
export WEECHAT_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/weechat"
export MBSYNCRC="${XDG_CONFIG_HOME:-$HOME/.config}/mbsync/config"
export ELECTRUMDIR="${XDG_DATA_HOME:-$HOME/.local/share}/electrum"
# export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java
# Other program settings:
export DICS="/usr/share/stardict/dic/"
export SDCV_PAGER='less --quit-if-one-screen -RX'
export SUDO_ASKPASS="$HOME/.local/bin/dmenupass"
export LESS=-R
export LESS_TERMCAP_mb="$(printf '%b' '[1;31m')"
export LESS_TERMCAP_md="$(printf '%b' '[1;36m')"
export LESS_TERMCAP_me="$(printf '%b' '[0m')"
export LESS_TERMCAP_so="$(printf '%b' '[01;44;33m')"
export LESS_TERMCAP_se="$(printf '%b' '[0m')"
export LESS_TERMCAP_us="$(printf '%b' '[1;32m')"
export LESS_TERMCAP_ue="$(printf '%b' '[0m')"
export LESSOPEN="| /usr/bin/highlight -O ansi %s 2>/dev/null"
export MOZ_USE_XINPUT2="1"		# Mozilla smooth scrolling/touchpads.
export AWT_TOOLKIT="MToolkit wmname LG3D"	#May have to install wmname
export _JAVA_AWT_WM_NONREPARENTING=1	# Fix for Java applications in dwm/bspwm

 [ ! -f ${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc ] && setsid shortcuts >/dev/null 2>&1


# [ "$(tty)" = "/dev/tty1" ] && exec sway
# Start graphical server on user's current tty if not already running and put the outputs into ~/.local/share/xorg/
[ "$(tty)" = "/dev/tty1" ] && ! pidof -s Xorg >/dev/null 2>&1 && exec startx "$XINITRC" -- -keeptty >~/.xorg.log 2>&1

# Switch escape and caps if tty and no passwd required:
# sudo -n loadkeys ${XDG_CONFIG_HOME:-$HOME/.config}/X11/ttymaps.kmap 2>/dev/null
