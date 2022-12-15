gxkb
volumeicon
battray
GRUB_CMDLINE_LINUX="resume=/dev/mapper/luks-62ac9270-fb7b-4fd0-8b38-80acb0178308 rd.luks.uuid=luks-137410e7-ed09-4ee7-9cf0-2ab0f6837153 rd.luks.uuid=luks-62ac9270-fb7b-4fd0-8b38-80acb0178308 rhgb quiet"
GRUB_CMDLINE_LINUX="resume=/dev/mapper/luks-62ac9270-fb7b-4fd0-8b38-80acb0178308 rd.luks.uuid=luks-137410e7-ed09-4ee7-9cf0-2ab0f6837153 rd.luks.uuid=luks-62ac9270-fb7b-4fd0-8b38-80acb0178308"
sudo grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg
sudo dnf remove plymouth*

sudo firewall-cmd --permanent --zone=public --add-service=kdeconnect
sudo firewall-cmd --reload

