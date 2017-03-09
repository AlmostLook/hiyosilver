#!/bin/bash


artist=`mpc -f %artist% | head -n 1`
title=`mpc -f %title% | head -n 1`
API_letra="https://makeitpersonal.co/lyrics/"
curl -s --get "$API_letra" --data-urlencode "artist=$artist" --data-urlencode "title=$title"  


echo -e "$artist - $title\n$song" | less -FX
