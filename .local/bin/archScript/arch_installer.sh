# == MY ARCH SETUP INSTALLER == #
#part1
printf '\033c'
echo "Welcome to Arch installer script"
sed -i "s/^#ParallelDownloads = 5$/ParallelDownloads = 15/" /etc/pacman.conf
pacman --noconfirm -Sy archlinux-keyring
loadkeys us
timedatectl set-ntp true
lsblk
echo "Enter the drive: "
read drive
cfdisk $drive 
echo "Enter the linux partition: "
read partition
mkfs.ext4 $partition 
read -p "Do you also want an efi partition? [y/n]" answer
if [[ $answer = y ]] ; then
  echo "Enter EFI partition: "
  read efipartition
  mkfs.vfat -F 32 $efipartition
fi
mount $partition /mnt 
pacstrap /mnt base base-devel linux linux-firmware
genfstab -U /mnt >> /mnt/etc/fstab
sed '1,/^#part2$/d' `basename $0` > /mnt/arch_install2.sh
chmod +x /mnt/arch_install2.sh
arch-chroot /mnt ./arch_install2.sh
exit 

#part2
printf '\033c'
pacman -S --noconfirm sed
sed -i "s/^#ParallelDownloads = 5$/ParallelDownloads = 15/" /etc/pacman.conf
ln -sf /usr/share/zoneinfo/Asia/Beirut /etc/localtime
hwclock --systohc
echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
echo "KEYMAP=us" > /etc/vconsole.conf
echo "Hostname: "
read hostname
echo $hostname > /etc/hostname
echo "127.0.0.1       localhost" >> /etc/hosts
echo "::1             localhost" >> /etc/hosts
echo "127.0.1.1       $hostname.localdomain $hostname" >> /etc/hosts
mkinitcpio -P
passwd
pacman --noconfirm -S grub efibootmgr os-prober
echo "Enter EFI partition: " 
read efipartition
mkdir /boot/efi
mount $efipartition /boot/efi 
grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=GRUB
sed -i 's/quiet/pci=noaer/g' /etc/default/grub
sed -i 's/GRUB_TIMEOUT=5/GRUB_TIMEOUT=3/g' /etc/default/grub
grub-mkconfig -o /boot/grub/grub.cfg

pacman -S --noconfirm zsh xorg-server xorg-xinit xorg-xkill xorg-xsetroot xorg-xprop \
xorg-xwininfo xdotool xorg-xdpyinfo xclip xorg-xset tlp pipewire \
pipewire-pulse wireplumber gnome-keyring xcape xorg-xev xorg-xinput \
xss-lock tmux numlockx cronie python-pip unclutter man-db dosfstools \
mtools socat exfat-utils picom libnotify exfat-utils ntfs-3g xcape \
xorg-xdpyinfo poppler mediainfo atool htop moreutils wget curl exfat-utils \
xterm redshift autoconf cmake perl-file-mimeinfo xdg-user-dirs unrar unzip \
xwallpaper ffmpeg maim mediainfo mpv yt-dlp ncmpcpp mpc mpd imagemagick \
ueberzug sxiv git-crypt git aria2 transmission-cli neomutt newsboat bc \
xsel clipmenu pulsemixer highlight fzf \
kbd poppler zathura zathura-pdf-mupdf feh mlocate android-tools \
xcolor bluez bluez-utils gnupg jq blueman patch make firefox mlocate bat \
ttf-dejavu ttf-liberation wqy-microhei noto-fonts-emoji ripgrep neovim \
dunst tk aspell brightnessctl isync pass

systemctl enable NetworkManager.service 
systemctl enable cronie.service 
systemctl enable tlp.service 
# rm /bin/sh
# ln -s dash /bin/sh
echo "%wheel ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
echo "Enter Username: "
read username
useradd -m -G wheel -s /bin/zsh $username
passwd $username
echo "Pre-Installation Finish Reboot now"
ai3_path=/home/$username/arch_install3.sh
sed '1,/^#part3$/d' arch_install2.sh > $ai3_path
chown $username:$username $ai3_path
chmod +x $ai3_path
su -c $ai3_path -s /bin/sh $username
exit 

#part3
printf '\033c'
cd $HOME
git clone --separate-git-dir=$HOME/.dotfiles https://github.com/mhdna/dotfiles.git tmpdotfiles
rsync --recursive --verbose --exclude '.git' tmpdotfiles/ $HOME/
rm -r tmpdotfiles
# dwm: Window Manager
git clone --depth=1 https://github.com/mhdna/dwm.git ~/.local/src/dwm
sudo make -C ~/.local/src/dwm install

# st: Terminal
git clone --depth=1 https://github.com/st/st.git ~/.local/src/st
sudo make -C ~/.local/src/st install

# dmenu: Program Menu
git clone --depth=1 https://github.com/dmenu/dmenu.git ~/.local/src/dmenu
sudo make -C ~/.local/src/dmenu install

# dmenu: Dmenu based Password Prompt
git clone --depth=1 https://github.com/ritze/pinentry-dmenu.git ~/.local/src/pinentry-dmenu
sudo make -C ~/.local/src/pinentry-dmenu clean install

# dwmblocks: Status bar for dwm
git clone --depth=1 https://github.com/mhdna/dwmblocks.git ~/.local/src/dwmblocks
sudo make -C ~/.local/src/dwmblocks install
# slock: Lock screen
git clone --depth=1 https://github.com/mhdna/slock.git ~/.local/src/slock
sudo make -C ~/.local/src/dwmblocks install
# mutt-wizard
git clone --depth=1 https://github.com/lukesmithxyz/mutt-wizard.git ~/.local/src/mutt-wizard
sudo make -C ~/.local/src/mutt-wizard install

# pikaur: AUR helper
git clone https://aur.archlinux.org/pikaur.git
cd pikaur
makepkg -fsri
cd
mkdir dl dox imp music pix pub code
mkdir -p .cache/thumbnails/mpv-gallery
# echo shutdown-hook \'\`pkill -RTMIN+12 dwmblocks\`\' >> ~/.config/mutt/muttrc

ln -s ~/.config/x11/xinitrc .xinitrc
ln -s ~/.config/shell/profile .zprofile
rm ~/.zshrc ~/.zsh_history
alias dots='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
dots config --local status.showUntrackedFiles no

crontab - < "~/.local/bin/archScript/cronjobs.txt"
pikaur -S - < ~/.local/bin/archScript/aur.txt
# You might need:
# sudo pacman -S --needed - < ~/.local/bin/archScript/packages.txt
# pip install pyright
# npm i -g vscode-langservers-extracted
# npm install -g typescript typescript-language-server
# npm install -g browser-sync
exit
