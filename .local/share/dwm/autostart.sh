#!/usr/bin/env bash

## run (only once) processes which spawn with the same name
function run {
   if (command -v $1 && ! pgrep $1); then
     $@&
   fi
}

# run cbatticon -r 1 -l 15 #-c 'loginctl poweroff'
# run pa-applet --disable-key-grabbing
# run blueman-applet
# run sb-mpdup
