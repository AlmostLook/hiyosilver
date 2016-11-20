#!/bin/bash

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
	[ -x "$f" ] && . "$f"
	done
	unset f
fi

#env
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
export LC_ALL="es_ES.UTF-8" &
setxkbmap -layout es &
xsetroot -cursor_name left_ptr &
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

#notify
exec trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 14 --height 24 --transparent true --alpha 0 --padding 2 --tint 0x1b1d1e &

exec xscreensaver -no-splash &
exec feh --bg-scale /home/x4w3/black-wallpaper-19.jpg &

if [ -x /usr/bin/nm-applet ] ; then
	nm-applet --sm-disable &
fi

exec volumeicon &
exec xfce4-power-manager & 
exec dropbox &
exec xfce4-clipman &

#play
exec xmonad
