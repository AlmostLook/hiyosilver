#!/bin/bash
while : ; do
     
	mpc idle &> /dev/null
	NOTIFY=$(mpc current)
	notify-send -i /usr/share/icons/gnome/16x16/actions/player_record.png ">>" "$NOTIFY" 
    purple-remote "setstatus?message=Playing now >> `mpc --format  "%artist% - %title%\nALBUM:%album% DATE:%date% GENRE:%genre%" | head -n2` << https://github.com/AlmostLook/hiyosilver" &
done

