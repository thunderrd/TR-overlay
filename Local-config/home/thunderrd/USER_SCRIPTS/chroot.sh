# mount 
mount /dev/sdf1 /mnt/aptosid-system
mount /dev/sdf5 /mnt/aptosid-home

#change dir
cd /mnt/aptosid-system

# mount temp api filesystems
mount -t proc proc proc/
mount --bind /sys sys/
mount --bind /dev dev/
mount --bind /run run/

# copy DNS info to dir
cp /etc/resolv.conf etc/resolv.conf

# chroot to a bash shell
chroot /mnt/aptosid-system /bin/bash

# load local bash configuration
source /etc/profile
source ~/.bashrc

# set a custom color prompt, so we don't forget where we are
#export PS1="\[\e[36;1m\]\u@\H:\[\e[31;1m\][CHROOT] \w# \[\e[0m\]"  		# normal prompt
#export PS1="\[\e[36;1m\]\u@\H:\[\e[31;5m\][CHROOT] \w# \[\e[0m\]"		# blinking [CHROOT]
export PS1="\e[31;1m\][CHROOT]\[\e[36;1m\]\u@\H:\[\e[31;1m\] \w# \[\e[0m\]"	# [CHROOT]root@Q6600: ~# no blink

# exit the chroot
# exit

# unmount the dirs
# umount -R /mnt/aptosid-system
# umount -R /mnt/aptosid-home


