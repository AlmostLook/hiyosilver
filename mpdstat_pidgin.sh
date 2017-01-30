#!/bin/bash
while : ; do
     
	mpc idle
	NOTIFY=$(mpc current)
	notify-send -i /usr/share/icons/gnome/16x16/actions/player_record.png "$NOTIFY"
    nohup purple-remote "setstatus?message=Reproduciendo >> `mpc current` << https://github.com/AlmostLook/hiyosilver"
done

