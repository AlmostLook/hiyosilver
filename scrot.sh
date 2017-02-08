#!/bin/bash
/usr/bin/scrot ~/Pictures/'%d-%m-%Y_%H-%M_$wx$h_scrot.png' -e '~/imgur/imgur  $f' && notify-send -t 2000 IMGUR 'URL en clipboard' && rm ~/Pictures/*
