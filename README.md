# Mahdi's Dotfiles

These are my dotfiles. I keep things as minimal as possible — no flashy colors, no distractions.

Tools that I use:

## Terminal Emulator: Suckless ST 
I used to have my own build, but as bugs started occuring especially with regards to sixel image generation, I started using [st-flexipatch](https://github.com/bakkeby/st-flexipatch).



## Window Manager: i3

A manual TWM is always better than a dynamic WM.

<img src="https://github.com/user-attachments/assets/0258b109-2957-4e2c-a602-6cad915a09cb" style="width:400px;">

<br/>

Along with i3blocks (scripts used are at bin/statusbar):


<img src="https://github.com/user-attachments/assets/2c30dfe2-dda2-42a3-b959-d4ee9a9db45f" style="width:800px;">


## File Manager: LF
LF is by far the most feature-rich terminal file manager I've ever used.
For image generation I use SIXEL, which is much better than using something like ueberzug.

<img src="https://github.com/user-attachments/assets/19e11d6a-ef7c-4f9f-84b8-9e924ce24da3" style="width:400px;">



## Text Editors 

For note taking I ues Emacs+org-mode.

For software development I've been using Neovim for 6 years now.

## Browser: Firefox+Arkenfox.js

I use my modified version of arkenfox (See: https://github.com/arkenfox/user.js/):

<img src="https://github.com/user-attachments/assets/65dffb15-4f64-4aeb-8a84-3ca0473781a1" style="width:400px;">

## Custom Keybindings
Using [keyd](https://github.com/rvaiya/keyd), I remap my capslock key to Control, and my rightalt to Super.

```
[main]
capslock = layer(control)
rightalt = layer(meta)
```

<details>
<summary><b>i3 Keybindings</b></summary>

| Keys                                        | Action                                                   |
|---------------------------------------------|----------------------------------------------------------|
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>Q</kbd> | Power menu                                     |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>X</kbd> | Kill window (`xkill`)                          |
| <kbd>Super</kbd> + <kbd>F1</kbd>            | `mykdc`                                                  |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>F1</kbd> | `mykdc -s`                                       |
| <kbd>Super</kbd> + <kbd>Z</kbd>             | Launch `boomer`                                         |
| <kbd>Super</kbd> + <kbd>Return</kbd>        | Launch terminal                                          |
| <kbd>Super</kbd> + <kbd>W</kbd>             | Launch `vuetify-api`                                     |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>W</kbd> | Focus/launch browser                           |
| <kbd>Super</kbd> + <kbd>T</kbd>             | Launch `transs`                                          |
| <kbd>Super</kbd> + <kbd>E</kbd>             | Emoji selector                                           |
| <kbd>Super</kbd> + <kbd>V</kbd>             | Clipmenu                                                 |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>V</kbd> | Pulsemixer                                      |
| <kbd>Super</kbd> + <kbd>C</kbd>             | Color picker                                             |
| <kbd>Super</kbd> + <kbd>N</kbd>             | Open `todo.org`                                          |
| <kbd>Super</kbd> + <kbd>O</kbd>             | Rerun menu-wrapper                                       |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>O</kbd> | Handler menu                                    |
| <kbd>Super</kbd> + <kbd>U</kbd>             | Drag clipboard URL with class                            |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>U</kbd> | Drag clipboard URL                              |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>N</kbd> | Open http://127.0.0.1:8087                      |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>R</kbd> | Open readlater                                         |
| <kbd>Super</kbd> + <kbd>P</kbd>             | Flameshot GUI                                            |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>P</kbd> | Screenshot menu                                 |
| <kbd>Super</kbd> + <kbd>Alt</kbd> + <kbd>P</kbd> | Delayed screenshot menu                          |
| <kbd>XF86AudioLowerVolume</kbd>             | Volume down 5%                                           |
| <kbd>XF86AudioRaiseVolume</kbd>             | Volume up 5%                                             |
| <kbd>Super</kbd> + <kbd>Minus</kbd>         | Volume down 5%                                           |
| <kbd>Super</kbd> + <kbd>Equal</kbd>         | Volume up 5%                                             |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>Minus</kbd> | Toggle mute                                   |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>Equal</kbd> | Volume max                                    |
| <kbd>Super</kbd> + <kbd>F9</kbd>            | `mpc-wrapper -r`                                         |
| <kbd>Super</kbd> + <kbd>F10</kbd>           | `mpc-wrapper -p`                                         |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>F10</kbd> | `mpc-wrapper -P`                               |
| <kbd>Super</kbd> + <kbd>F11</kbd>           | `mpc-wrapper -t`                                         |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>F11</kbd> | Pause all players                             |
| <kbd>Super</kbd> + <kbd>F12</kbd>           | `mpc-wrapper -n`                                         |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>F12</kbd> | `mpc-wrapper -N`                               |
| <kbd>Super</kbd> + <kbd>Backspace</kbd>     | Close all notifications                                  |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>Backspace</kbd> | Dunst history pop                              |
| <kbd>Super</kbd> + <kbd>Alt</kbd> + <kbd>Backspace</kbd> | Toggle Dunst paused                              |
| <kbd>Super</kbd> + <kbd>Comma</kbd>         | Brightness down 5%                                       |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>Comma</kbd> | Brightness 1%                                 |
| <kbd>Super</kbd> + <kbd>Period</kbd>        | Brightness up 5%                                         |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>Period</kbd> | Brightness 100%                               |
| <kbd>Super</kbd> + <kbd>F5</kbd>            | Set brightness via `tee`                                 |
| <kbd>Super</kbd> + <kbd>I</kbd>             | Rescan Wi-Fi and wait                                    |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>I</kbd> | Launch `nmtui`                                  |
| <kbd>Super</kbd> + <kbd>Insert</kbd>        | `transs -p`                                               |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>Insert</kbd> | `transs -c`                                    |
| <kbd>Super</kbd> + <kbd>Delete</kbd>        | `transs -ap`                                              |
| <kbd>Super</kbd> + <kbd>Shift</kbd> + <kbd>Delete</kbd> | `transs -ac`                                    |

</details>

<details>
<summary><b>tmux Keybindings</b></summary>

| Keys                        | Action                                                      |
|-----------------------------|-------------------------------------------------------------|
| <kbd>M-;</kbd>              | Split window (vertical) in current path                     |
| <kbd>M-:</kbd>              | Split window (horizontal) in current path                   |
| <kbd>C-1</kbd> to <kbd>C-8</kbd> | Go to or create window 1–8 in current path             |
| <kbd>C-9</kbd>              | Go to last window                                           |
| <kbd>M-1</kbd> to <kbd>M-8</kbd> | Go to or create window 1–8 in current path             |
| <kbd>M-9</kbd>              | Go to last window                                           |
| <kbd>M-i</kbd>              | Go to last used window                                      |
| <kbd>M-w</kbd>              | Show `popup-main`                                           |
| <kbd>M-l</kbd>              | Scratchpad: `lfrun`                                         |
| <kbd>M-q</kbd>              | Detach client                                               |
| <kbd>C-t</kbd>              | Rotate window                                               |
| <kbd>C</kbd>                | New window in current path (prefix + C)                    |
| <kbd>M-o</kbd>              | New window with `htop`                                      |
| <kbd>y</kbd> (in copy mode) | Copy selection using `xclip-or-wl-copy`                    |

</details>

<details>
<summary><b>lf Keybindings</b></summary>

| Keys                 | Action                                             |
|----------------------|----------------------------------------------------|
| <kbd>DD</kbd>        | Move to trash (`trash-put`)                        |
| <kbd>DL</kbd>        | Delete permanently                                 |
| <kbd>DS</kbd>        | Shred file                                         |
| <kbd>TT</kbd>        | Go to trash folder                                 |
| <kbd>TR</kbd>        | Clear screen and restore from trash                |
| <kbd>TE</kbd>        | Clear screen and empty trash                       |
| <kbd>Z</kbd>         | Convert PDF (`pdfConvert`)                         |
| <kbd>Y</kbd>         | Copy to                                            |
| <kbd>M</kbd>         | Move to                                            |
| <kbd>Alt+M</kbd>     | Move to                                            |
| <kbd>o</kbd>         | Open with default app                              |
| <kbd>O</kbd>         | Open with menu                                     |
| <kbd>Ctrl+O</kbd>    | Ask default open with mimeopen                     |
| <kbd>x</kbd>         | Execute file with `!clear; $f`                     |
| <kbd>X</kbd>         | `chmod`                                            |
| <kbd>w</kbd>         | Open terminal with setsid                          |
| <kbd>U</kbd>         | Show dir size (`du -sh`)                           |
| <kbd>u</kbd>         | Unselect                                           |
| <kbd>Alt+R</kbd>     | Reload config (`so`)                               |
| <kbd>Ctrl+R</kbd>    | Reload and redraw                                  |
| <kbd>Ctrl+G</kbd>    | Glob-select all                                    |
| <kbd>Ctrl+J</kbd>    | `zi`                                               |
| <kbd>Ctrl+F</kbd>    | `:fzf_jump`                                        |
| <kbd>gs</kbd>        | `:fzf_search`                                      |
| <kbd>Ctrl+Space</kbd>| Toggle select and move up                          |
| <kbd>/</kbd>         | Search forward                                     |
| <kbd>?</kbd>         | Search backward                                    |
| <kbd>Alt+/</kbd>     | Search forward                                     |
| <kbd>Alt+?</kbd>     | Search backward                                    |
| <kbd>K</kbd>         | Launch `$kdcfiles-menu` with selection             |
| <kbd>P</kbd>         | Run `$myscp` with selection                        |
| <kbd>)</kbd>         | Upload to 0x0                                      |
| <kbd>A</kbd>         | Rename (end)                                       |
| <kbd>a</kbd>         | Rename (after extension)                           |
| <kbd>I</kbd>         | Rename (start)                                     |
| <kbd>i</kbd>         | Rename (before extension)                          |
| <kbd>r</kbd>         | Push rename command                                |
| <kbd>B</kbd>         | Bulk rename                                        |
| <kbd>b</kbd>         | Set wallpaper/background                           |
| <kbd>Alt+E</kbd>     | Echo filename                                      |
| <kbd>L</kbd>         | Create link                                        |
| <kbd>V</kbd>         | Invert below                                       |
| <kbd>yy</kbd>        | Copy                                               |
| <kbd>yp</kbd>        | Yank path                                          |
| <kbd>yn</kbd>        | Yank name                                          |
| <kbd>yP</kbd>        | Yank full path                                     |
| <kbd>Ctrl+A Z</kbd>  | Zip file                                           |
| <kbd>Ctrl+A T</kbd>  | Tar file                                           |
| <kbd>Ctrl+A G</kbd>  | Tar.gz file                                        |
| <kbd>Ctrl+A B</kbd>  | Tar.bz2 file                                       |
| <kbd>E</kbd>         | Extract                                            |
| <kbd>m</kbd>         | Make directory                                     |
| <kbd>f</kbd>         | Create file                                        |
| <kbd>S</kbd>         | Select all files                                   |
| <kbd>R</kbd>         | Resize image                                       |
| <kbd>Cr</kbd>        | Convert image to JPG                               |
| <kbd>Co</kbd>        | Compress video                                     |
| <kbd>sS</kbd>        | Strip whitespace                                   |
| <kbd>Tab</kbd>       | Prepend line in file (`replace` command)           |
| <kbd>dd</kbd>        | Cut                                                |
| <kbd>dr</kbd>        | Drag-and-drop with `dragon`                        |
| <kbd>dm</kbd>        | Move via dragon                                    |
| <kbd>dc</kbd>        | Copy via dragon                                    |
| <kbd>dl</kbd>        | Download file (drag-related)                       |
| <kbd>P</kbd>         | Open current dir in pcmanfm                        |

</details>

<details>
<summary><b>Emacs (Evil) Keybindings</b></summary>

| Keys           | Action                        |
| -------------- | ----------------------------- |
| Alt+Space      | (insert) — disabled           |
| Ctrl+U         | (insert) — kill to line start |
| Alt+U          | Universal argument            |
| Alt+Shift+U    | Negative argument             |
| Alt+D          | Duplicate line                |
| z d            | Dictionary lookup             |
| SPC w          | Save buffer                   |
| SPC W          | Write file                    |
| SPC =          | Indent buffer                 |
| -              | Open dired in current dir     |
| SPC a          | Open org-agenda               |
| SPC B          | Magit blame toggle            |
| SPC g          | Magit status                  |
| SPC y          | Yank history popup            |
| gs             | Ripgrep search                |
| SPC R          | Find file as root             |
| SPC u          | Undo tree visualize           |
| SPC M          | Open new frame                |
| SPC O          | Open URL with xdg-open        |
| SPC f          | Find file                     |
| SPC v          | Find alternate file           |
| SPC b          | Switch buffer                 |
| SPC r          | Recent files                  |
| SPC j          | Jump to bookmark              |
| SPC J          | Set bookmark                  |
| SPC k          | Close current buffer          |
| Ctrl+Shift+T   | Open last closed              |
| SPC P          | Project switch                |
| SPC p          | Project find file             |
| SPC c          | Org capture                   |
| SPC C          | Open calc                     |
| SPC d          | Open diary                    |
| SPC D          | Open journal                  |
| SPC t          | Open org TODO                 |
| SPC s          | Open vterm                    |
| SPC T          | Org capture TODO              |
| SPC n          | Open notes                    |
| SPC R          | Reload init file              |
| SPC o (html)   | Open buffer URL (html-mode)   |
| SPC / (org)    | Org sparse tree               |
| Alt+P (org)    | Push Anki notes               |
| SPC e (org)    | Emphasize                     |
| Alt+B (org)    | Emphasize bold                |
| SPC A (org)    | Mark done and archive         |
| SPC i (org)    | Org goto                      |
| SPC E (org)    | Show inline images            |
| SPC xi         | Org clock-in                  |
| SPC xo         | Org clock-out                 |
| SPC xx         | Org clock display             |
| SPC l          | Org store link                |
| SPC L          | Org insert link               |
| SPC o (org)    | Org open at point             |
| SPC e (elisp)  | Eval last sexp                |
| SPC E (elisp)  | Eval region or buffer         |
| (              | Prev open paren (elisp)       |
| )              | Next close paren (elisp)      |
| Ctrl+C (minib) | Abort minibuffer              |
| Ctrl+C (ins)   | Exit insert state             |
| Ctrl+C (vis)   | Exit visual state             |

</details>



