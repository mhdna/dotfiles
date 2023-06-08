#!/usr/bin/env bash

## run (only once) processes which spawn with the same name
function run {
	 if (command -v $1 && ! pgrep $1); then
		 $@&
	 fi
}
setbg&
tint2&
pnmixer&
