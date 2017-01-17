## !/bin/sh
#
## ~/.xinitrc   [For Xfce logon session.  Invokes ~/.gtkrc-2.0 and ~/.Xdefaults]


## This file can export environment variables that affect apps running over X,
## start daemons like the screensaver, and finally start the Window Manager


# starts the urxvt daemon
urxvtd -q -f -o


# Let Qt find the GTK config
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"


## The following 3 lines handle the .Xdefaults/.Xresources linkage [colors, fonts, and positions for the terminals urxvt and xterm]
if [ -f ~/.Xdefaults ]; then
  xrdb -merge /home/thunderrd/.Xdefaults
fi


# sets nvidia thermal applet to 'on', and fan control to 'x'%
nvidia-settings -a [gpu:0]/GPUFanControlState=1
nvidia-settings -a [fan:0]/GPUTargetFanSpeed=65		##syntax for newer kernels


# starts the polipo web cache proxy for torbrowser, second instance [non-root], with proxyPort=8124 (root instance is 8123, default)
#the root instance is started from /etc/init.d/polipo, for non-tor web browser
#/usr/bin/polipo -c /etc/polipo/config-torbrowser &


# the following line starts xfce
exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xfce4-session


## Experimental startup lines
#exec ck-launch-session dbus-launch --sh-syntax --exit-with-session startxfce4
#exec dbus-launch --sh-syntax --exit-with-session xfce4-session
#exec startxfce4 --with-ck-launch
#exec ck-launch-session dbus-launch --sh-syntax xfce4-session
