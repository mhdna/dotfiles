sudo nvim /etc/systemd/system.conf
DefaultTimeoutStopSec=5s
  ==> ignore the power
sed -i "s/^#HandlePowerKey=poweroff/HandlePowerKey=ignore/" /etc/elogind/logind.conf

change default kernel
# == ARCH Installation Script == #
#part1
printf '\033c'
echo "Installing Arch(arch) or Artix-runit/Artix-openrc(artixr, artixo)?"
read distro
sed -i "s/^#ParallelDownloads = 5$/ParallelDownloads = 5/" /etc/pacman.conf

location = Asia/Beirut

# Artix
if [[ $distro = artixr ]]
		initS = runit elogind-runit networkmanager-runit artix-archlinux-support
		keyring = artix-keyring
		pacinstall = basestrap
		genfstab = fstabgen
		chroot = artix-chroot
if [[ $distro = artixo ]]
		initS = openrc elogind-openrc networkmanager-openrc artix-archlinux-support
		keyring = artix-keyring
		pacinstall = basestrap
		genfstab = fstabgen
		chroot = artix-chroot
	# Arch
elif [[ $distro = arch ]]
	init = ' '
	keyring = archlinux-keyring
	pacinstall = pacstrap
	genfstab = genfstab
	chroot = arch-chroot
fi

pacman --noconfirm -Sy $keyring
loadkeys us
timedatectl set-ntp true
read -p "Did you wifi or ethernet? [w/e]" answer

if [[ $answer = w ]] ; then
  rfkill unblock wifi

  echo "Enter wifi device name partition: "

  if [[ $distro = arch ]] ; then
	  iwctl device list
	  read wlan
	  iwctl station $wlan scan
	  iwctl station get-networks
	  echo "Enter SSID: "
	  read SSID
	  iwctl station connect $SSID
  fi

  if [[ $distro = artix ]] ; then
	  connmanctl scan wifi
	  connmanctl services
	  echo "Enter your wifi device's code"
	  read $CODE
	  connmanctl connect $CODE
fi

# partitioning
lsblk
echo "Enter the drive: "
read drive
echo "You want to partition(Y/n)?"
read -p "Did you want to partition the disk? [y/n]" answer
if [[ $answer = y ]] ; then
cfdisk $drive
fi

echo "Enter the root partition (/dev/***): "
read root
mkfs.ext4 $root
read -p "Do you want to create an efi partition? [y/n]" answer
if [[ $answer = y ]] ; then
	echo "Enter EFI partition (/dev/***): "
  read efipartition
  mkfs.vfat -F 32 $efipartition
fi

mount $partition /mnt

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

$pacinstall /mnt base base-devel linux linux-firmware neovim

$genfstab -U /mnt >> /mnt/etc/fstab
sed '1,/^#part2$/d' `basename $0` > /mnt/arch_install2.sh
chmod +x /mnt/arch_install2.sh
$chroot /mnt ./arch_install2.sh
exit

#part2
printf '\033c'
pacman -S --noconfirm sed
sed -i "s/^#ParallelDownloads = 5$/ParallelDownloads = 5/" /etc/pacman.conf
sed -i "s/^#Color$/Color/" /etc/pacman.conf
if [[ $distro = artixo || $distro = artixr]]
	echo "
# Arch
[extra]
Include = /etc/pacman.d/mirrorlist-arch

[community]
Include = /etc/pacman.d/mirrorlist-arch

# [multilib]
# Include = /etc/pacman.d/mirrorlist-arch
" >> /etc/pacman.conf

ln -sf /usr/share/zoneinfo/$location /etc/localtime
hwclock --systohc

# Echoing stuff
echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
echo "KEYMAP=us" > /etc/vconsole.conf
mkdir -p /etc/sysctl.d
echo  "kernel.sysrq = 1" >> /etc/sysctl.d/99-sysctl.conf
# disable beep
rmmod pcspkr && echo "blacklist pcspkr" >> /etc/modprobe.d/nobeep.conf
sed -i "s/Y/N/" /sys/module/snd_hda_intel/parameters/power_save_controller
sed -i "s/1/0/" /sys/module/snd_hda_intel/parameters/power_save
echo "auth     optional  pam_gnupg.so store-only
session  optional  pam_gnupg.so" >> /etc/pam.d/system-local-login
echo "Section \"Device\"
    Identifier \"Intel Graphics\"
    Driver \"intel\"
    Option \"TearFree\" \"true\"
EndSection" >> /etc/X11/xorg.conf.d/20-intel.conf


echo "Enter Your Hostname: "
read hostname
echo $hostname > /etc/hostname
echo "127.0.0.1       localhost
::1             localhost
127.0.1.1       $hostname.localdomain $hostname" >> /etc/hosts
mkinitcpio -P
echo "Enter a root password"
passwd
pacman --noconfirm -S grub efibootmgr os-prober

echo "Enter EFI partition: "
read efipartition
mkdir /boot/efi
mount $efipartition /boot/efi
grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=GRUB
sed -i 's/quiet/pci=noaer/g' /etc/default/grub
sed -i 's/GRUB_TIMEOUT=5/GRUB_TIMEOUT=1/' /etc/default/grub
sed -i 's/#GRUB_DISABLE_OS_PROBER=false/GRUB_DISABLE_OS_PROBER=false/' /etc/default/grub
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
	ln -s /etc/runit/sv/NetworkManager /etc/runit/runsvdir/current
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
