#!/bin/bash

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
	[ -x "$f" ] && . "$f"
	done
	unset f
fi

#env
#eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
export LC_ALL="es_ES.UTF-8" &
setxkbmap -layout es &
xsetroot -cursor_name left_ptr &
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

#screen
#sh /home/x4w3/.screenlayout/resolution.sh &

#notify
(sleep 5 && trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 12 --height 25 --transparent true --alpha 0 --padding 2 --tint 0x1b1d1e) &

#mpd /home/x4w3/.mpd/mpd.conf
xscreensaver -no-splash &
feh --bg-scale /home/n3w4x/black-wallpaper-19.jpg &

if [ -x /usr/bin/nm-applet ] ; then
	nm-applet --sm-disable &
fi

volumeicon &
xfce4-power-manager & 
dropbox &
xfce4-clipman &
tomboy &
orage &
pidgin &
mpdstat_pidgin &
steam &

#play
exec xmonad
