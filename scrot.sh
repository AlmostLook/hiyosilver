#!/bin/bash

#Script SCROT-IMGUR

/usr/bin/scrot ~/Pictures/'%d-%m-%Y_%H-%M_$wx$h_scrot.png' -e '/home/n3w4x/imgur/imgur  $f' && notify-send -t 2000 IMGUR 'URL en clipboard' && rm ~/Pictures/*
