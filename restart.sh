#!/bin/bash

notify-send -i "gnome-power-manager.png" "Xmonad" "Reiniciando el sistema"

mpc stop
pkill volumeicon
wait
pkill dropbox
wait
pkill xfce4-clipman
wait
pkill tomboy
wait
pkill orage
wait
pkill mpdstat_pidgin
wait
pkill pidgin
wait
pkill steam
wait
sleep 2
pkill conky && killall dzen2
notify-send -i "gnome-power-manager.png" "Xmonad" "Bye Bye!!"
sleep 2
systemctl reboot -i
