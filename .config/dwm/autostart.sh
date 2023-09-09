#!/usr/bin/env bash

## run (only once) processes which spawn with the same name
function run {
	 if (command -v $1 && ! pgrep $1); then
		 $@ > /dev/null 2>&1 &
	 fi
}
killall -q dwmblocks;  dwmblocks&
run picom # in case it stopped while restarting
setbg
pkill -USR1 sxhkd || sxhkd
