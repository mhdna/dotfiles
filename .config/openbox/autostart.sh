#!/usr/bin/env bash

## run (only once) processes which spawn with the same name
function run {
   if (command -v $1 && ! pgrep $1); then
     $@&
   fi
}
# run tint2
# run setbg
# run cbatticon
# run nm-applet
# run pa-applet --disable-key-grabbing
# run fbxkb
run xclock -bg white && xdo lower -N XClock
