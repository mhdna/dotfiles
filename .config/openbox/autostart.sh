#!/usr/bin/env bash

## run (only once) processes which spawn with the same name
function run {
   if (command -v $1 && ! pgrep $1); then
     $@&
   fi
}
dpselector 2&
setwall&
tint2&
sleep 2s &&
$TERMINAL -c Tmux -e sh -c 'tmux a || tmux'&
emacs&
fol ncmpcpp&
fol lfub&
fol sudo sudo su&
$BROWSER&

