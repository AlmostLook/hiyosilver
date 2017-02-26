#!/bin/bash

background="#000000"
foreground="#ffffff"
highlight="#ff5f00"

FONT="-*-inconsolata-medium-*-normal-*-14-*-*-*-*-*-*-*"

artist=`mpc current -f %artist%`
title=`mpc current -f %title%`
API_letra="https://makeitpersonal.co/lyrics/"

lyrics=$(curl -s --get $API_letra --data-urlencode "artist=$artist" --data-urlencode "title=$title") 
(echo "$lyrics"; sleep 60) | dzen2 -fg $foreground -bg $background -fn $FONT -x 0 -y 1050 -w 1920 -l 1 -e 'onstart=uncollapse,hide;button1=exit;button3=exit;button4=scrollup;button5=scrolldown;'
