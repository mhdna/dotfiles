# == Artix Installation Script == #
#part1
printf '\033c'
echo "Welcome to Arch Installation Script"
sed -i "s/^#ParallelDownloads = 5$/ParallelDownloads = 5/" /etc/pacman.conf

location = Asia/Beirut

# echo "Use runit or openrc?"
# read init
#
# if [[ $init = artix ]]
# 		initS = runit elp-runit elogind-runit artix-archlinux-support networkamanger-runit
# if [[ $distro = artix ]]
# 		initS = openrc elogind-openrc artix-archlinux-support networkmanager-openrc
# 	# artix
# elif [[ $distro = arch ]]
# 	init = ' '
# 	keyring = archlinux-keyring
# 	pacinstall = pacstrap
# 	genfstab = genfstab
# 	chroot = arch-chroot
# fi
#
pacman --noconfirm -Sy artix-keyring
loadkeys us
timedatectl set-ntp true
read -p "Did you want to use wifi? [y/n]" answer

if [[ $answer = y ]] ; then
    rfkill unblock wifi
    echo "Enter your wifi SSID: "
    # if [[ $distro = arch ]] ; then
    #  iwctl device list
    #  read wlan
    #  iwctl station $wlan scan
    #  iwctl station get-networks
    #  echo "Enter SSID: "
    #  read SSID
    #  iwctl station connect $SSID
    # fi
    #
    # if [[ $distro = artix ]] ; then
    connmanctl scan wifi
    connmanctl services
    echo "Enter your wifi device's code"
    read $CODE
    connmanctl connect $CODE
    fi

# partitioning
# TODO: partitioning
lsblk
echo "Which drive? "
read drive
read -p "Did you want to partition the disk? [y/n]" answer
if [[ $answer = y ]] ; then
    fdisk $drive
fi

echo "Which partition to use as root (/dev/***): "
read root
mkfs.ext4 $root
read -p "Do you want to create an efi partition? [y/n]" answer
if [[ $answer = y ]] ; then
    echo "Enter EFI partition (/dev/***): "
    read efipartition
    mkfs.vfat -F 32 $efipartition
fi

mount $partition /mnt
mount $efipartition /boot/efi

read -p "(mount) or (create) a \"home\" partition or (none)? [m/c/n]" answer
if [[ $answer = m ]] ; then
    echo "Enter HOME partition (/dev/***): "
    read homepartition
    mkdir /mnt/home
    mount $homepartition /mnt/home
elif [[ $answer = c ]]; then
    read -p "This will wipe out your partition, create? [y/n]" answer
    if [[ $answer = y ]] ; then
        echo "Enter HOME partition (/dev/***): "
        read homepartition
        mkfs.ext4 $homepartition
        mkdir /mnt/home
        mount $homepartition /mnt/home
    fi
fi

basestrap /mnt base base-devel linux linux-firmware vi runit tlp-runit elogind-runit networkamanger-runit sed

fstabgen -U /mnt >> /mnt/etc/fstab
sed '1,/^#part2$/d' `basename $0` > /mnt/arch_install2.sh
chmod +x /mnt/arch_install2.sh
artix-chroot /mnt ./arch_install2.sh
exit

#part2
printf '\033c'

ln -sf /usr/share/zoneinfo/$location /etc/localtime
hwclock --systohc

# System config files
sed -i "s/^#ParallelDownloads = 5$/ParallelDownloads = 10/" /etc/pacman.conf
sed -i "s/^#Color$/Color/" /etc/pacman.conf
echo "
[universe]
Server = https://universe.artixlinux.org/\$arch
Server = https://mirror1.artixlinux.org/universe/\$arch
Server = https://mirror.pascalpuffke.de/artix-universe/\$arch
Server = https://artixlinux.qontinuum.space/artixlinux/universe/os/\$arch
Server = https://mirror1.cl.netactuate.com/artix/universe/\$arch
Server = https://ftp.crifo.org/artix-universe/

# Arch
[extra]
Include = /etc/pacman.d/mirrorlist-arch

[community]
Include = /etc/pacman.d/mirrorlist-arch

# [multilib]
# Include = /etc/pacman.d/mirrorlist-arch
" >> /etc/pacman.conf
echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
echo "KEYMAP=us" > /etc/vconsole.conf
echo  "kernel.sysrq = 1" >> /etc/sysctl.d/99-sysctl.conf
rmmod pcspkr && echo "blacklist pcspkr" >> /etc/modprobe.d/nobeep.conf
sed -i "s/#SOUND_POWER_SAVE_ON_AC=1/SOUND_POWER_SAVE_ON_AC=0/" /etc/tlp.conf
sed -i "s/#SOUND_POWER_SAVE_ON_BAT=1/SOUND_POWER_SAVE_ON_BAT=0/" /etc/tlp.conf
# sed -i "s/Y/N/" /sys/module/snd_hda_intel/parameters/power_save_controller
# sed -i "s/1/0/" /sys/module/snd_hda_intel/parameters/power_save
sed -i "s/#HandlePowerKey=poweroff/HandlePowerKey=ignore/" /etc/elogind/logind.conf
echo "auth     optional  pam_gnupg.so store-only
session  optional  pam_gnupg.so" >> /etc/pam.d/system-local-login
# optional
sudo echo "password  optional        pam_gnome_keyring.
" >> /etc/pam.d/passwd
# echo "auth     optional  pam_gnupg.so" >> /etc/pam.d/i3lock
# echo "Section "Device"
#     Identifier "Intel Graphics"
#     Driver "intel"
#     Option "TearFree" "true"
# EndSection
# " >> /etc/X11/xorg.conf.d/20-intel.conf

echo "Enter Your Hostname: "
read hostname
echo $hostname > /etc/hostname
echo "127.0.0.1       localhost
::1             localhost
127.0.1.1       \$hostname.localdomain \$hostname" >> /etc/hosts
mkinitcpio -P
echo "Enter a root password"
read passed
sudo passwd $passwd
grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=GRUB
sed -i 's/quiet/pci=noaer/g' /etc/default/grub
sed -i 's/GRUB_TIMEOUT=5/GRUB_TIMEOUT=0/' /etc/default/grub
sed -i 's/#GRUB_DISABLE_OS_PROBER=false/GRUB_DISABLE_OS_PROBER=false/' /etc/default/grub
sed -i 's/#GRUB_DISABLE_OS_PROBER=false/GRUB_DISABLE_OS_PROBER=false/' /etc/default/grub
sed -i 's/GRUB_DEFAULT=0/GRUB_DEFAULT="Advanced options for Artix Linux>Artix Linux, with Linux linux"/' /etc/default/grub
grub-mkconfig -o /boot/grub/grub.cfg

echo "Arch(arch) or Artix(artixr, artixo)?"
read distro

echo "Full or minimal install? [f/m]"
read answer
if [[ $answer = f]]
    pacman -S --noconfirm --needed - < packages.txt

elif [[ $answer = m]]
    pacman -S --noconfirm --needed - < packagesmini.txt
fi

if [[ $distro = arch]]
    systemctl enable NetworkManager.service
    systemctl enable cronie.service
elif [[ $distro = artixr]]
    ln -s /etc/runit/sv/Networkmanager /etc/runit/runsvdir/current
    ln -s /etc/runit/sv/cronie /etc/runit/runsvdir/current
elif [[ $distro = artixo]]
    rc-update add NetworkManager default
    rc-update add cronie default
fi

# rm /bin/sh
# ln -s zsh /bin/sh
echo "%wheel ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
crontab - < "cronjobs.txt"

echo "Enter Username: "
read username
read -p "create home directory? [y/n]" answer
if [[ $answer = y]]
    useradd -m -G wheel -s /bin/zsh $username
else
    useradd -G wheel -s /bin/zsh $username
fi

passwd $username
echo "Pre-Installation Finished Reboot now"
ai3_path=/home/$username/arch_install3.sh
sed '1,/^#part3$/d' arch_install2.sh > $ai3_path
chown $username:$username $ai3_path
chmod +x $ai3_path
su -c $ai3_path -s /bin/sh $username
exit

#part3
printf '\033c'
cd $HOME
git clone --separate-git-dir=$HOME/.dotfiles https://github.com/bugswriter/dotfiles.git tmpdotfiles
rsync --recursive --verbose --exclude '.git' tmpdotfiles/ $HOME/
rm -r tmpdotfiles
# dwm: Window Manager
git clone --depth=1 https://github.com/Bugswriter/dwm.git ~/.local/src/dwm
sudo make -C ~/.local/src/dwm install

# st: Terminal
git clone --depth=1 https://github.com/Bugswriter/st.git ~/.local/src/st
sudo make -C ~/.local/src/st install

# dmenu: Program Menu
git clone --depth=1 https://github.com/Bugswriter/dmenu.git ~/.local/src/dmenu
sudo make -C ~/.local/src/dmenu install

# dmenu: Dmenu based Password Prompt
git clone --depth=1 https://github.com/ritze/pinentry-dmenu.git ~/.local/src/pinentry-dmenu
sudo make -C ~/.local/src/pinentry-dmenu clean install

# dwmblocks: Status bar for dwm
git clone --depth=1 https://github.com/bugswriter/dwmblocks.git ~/.local/src/dwmblocks
sudo make -C ~/.local/src/dwmblocks install

# pikaur: AUR helper
git clone https://aur.archlinux.org/pikaur.git
cd pikaur
makepkg -fsri
cd
pikaur -S
# mkdir dl dox imp music pix pub code

ln -s ~/.config/x11/xinitrc .xinitrc
ln -s ~/.config/shell/profile .zprofile
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# mv ~/.oh-my-zsh ~/.config/zsh/oh-my-zsh
# rm ~/.zshrc ~/.zsh_history
alias dots='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
dots config --local status.showUntrackedFiles no
exit


# creating  symlinks
ln -s /media/dox ~/dox
ln -s /media/pix ~/pix
ln -s /media/vids ~/vids
ln -s /media/music ~/music
ln -s /media/.local/share/transmission ~/.local/share/transmission
## for mpv error
mkdir -p .cache/thumbnails/mpv-gallery
# slock
cd ~/.local/src/slock/
sudo make install

ln -s /etc/runit/sv/backlight /etc/runit/runsvdir/current
ln -s /etc/runit/sv/cronie /etc/runit/runsvdir/current
ln -s /etc/runit/sv/tlp /etc/runit/runsvdir/current


# 2 --> add user-prefs.js for firefox

# some configs
echo shutdown-hook \'\`pkill -RTMIN+12 dwmblocks\`\' >> ~/.config/mutt/muttrc
