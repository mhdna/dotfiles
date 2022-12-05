#!/usr/bin/env bash

## run (only once) processes which spawn with the same name
function run {
   if (command -v $1 && ! pgrep $1); then
     $@&
   fi
}
# run xrdb ${XDG_CONFIG_HOME:-$HOME/.config}/x11/xresources
# run /usr/bin/lxpolkit
# run /usr/lib/kdeconnectd
# run picom
# run xfce4-screensaver
run cbatticon -r 1 -l 15 #-c 'loginctl poweroff'
run pa-applet --disable-key-grabbing
run nm-applet
#run alttab
# run xfce4-power-manager
#--no-daemon

# run copyq
# run workrave
# run dwmblocks &
# run sb-mpdup
# python3 -m radicale --config "~/.config/radicale/config"&
# run fbxkb
# run "dex $HOME/.config/autostart/arcolinux-welcome-app.desktop"
#run "xrandr --output VGA-1 --primary --mode 1360x768 --pos 0x0 --rotate normal"
#run "xrandr --output HDMI2 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off"
#run xrandr --output eDP-1 --primary --mode 1368x768 --pos 0x0 --rotate normal --output DP-1 --off --output HDMI-1 --off --output DP-2 --off --output HDMI-2 --off
#run xrandr --output LVDS1 --mode 1366x768 --output DP3 --mode 1920x1080 --right-of LVDS1
#run xrandr --output DVI-I-0 --right-of HDMI-0 --auto
#run xrandr --output DVI-1 --right-of DVI-0 --auto
#run xrandr --output DVI-D-1 --right-of DVI-I-1 --auto
#run xrandr --output HDMI2 --right-of HDMI1 --auto
#autorandr horizontal

# run "pamac-tray"
# run "variety"
# run "xfce4-power-manager"
# run "blueberry-tray"
# run "/usr/lib/xfce4/notifyd/xfce4-notifyd"
# run "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
# picom -b  --config ~/.config/arco-dwm/picom.conf &
# run "numlockx on"
# run "volumeicon"
# run slstatus &
# sxhkd -c ~/.config/arco-dwm/sxhkd/sxhkdrc &
##run "nitrogen --restore"
#run "conky -c $HOME/.config/arco-dwm/system-overview"
##you can set wallpapers in themes as well
# feh --bg-fill /usr/share/backgrounds/arcolinux/arco-wallpaper.jpg &

#run applications from startup

#run "insync start"
#run "spotify"
#run "ckb-next -b"
#run "discord"
#run "telegram-desktop"
