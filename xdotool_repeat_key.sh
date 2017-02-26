#!/bin/bash

#wnd_id=$(xdotool search --name "manaplus")
#wnd_name=$(xdotool getwindowname $wnd_id)
#wnd_title=$(xprop -id $wnd_id WM_NAME)    
#wnd_focus=$(xdotool getwindowfocus) 
#stty -echo -icanon -icrnl time 0 min 0

while true; do
	sleep 1
  	xdotool search --name manaplus key Control_L click 1
	sleep 0.6
	xdotool search --name manaplus key Shift_L

#xdotool search --name manaplus key Shift_L  
done &

