#!/bin/bash
#
# This scripts downloads today's sunrise or sunset times from weather.yahoo.com
# You'll need to obtain a location ID from weather.yahoo.com for your location
# Go to http://weather.yahoo.com and enter your city or zip code a new URL opens
# EXAMPLE: http://weather.yahoo.com/nederland/noord-brabant/tilburg-733881/ 
# In the example 733881 is the location ID for my location...

# Example parameters:
# thisscript.sh sunset # shows today's sunset
# thisscript.sh sunrise # shows today's sunrise
# thisscript.sh sunset \"30 minutes\" # shows today's sunset +30 minutes
# thisscript.sh sunset \"2 hours\" # shows today's sunset +2 hours
# thisscript.sh sunset \"30 minutes ago\" # shows today's sunset -30 minutes
# thisscript.sh sunset \"2 hours ago\" # shows today's sunset -2 hours
# thisscript.sh sunrise \"30 minutes\" # shows today's sunrise +30 minutes
# thisscript.sh sunrise \"2 hours\" # shows today's sunrise +2 hours
# thisscript.sh sunrise \"30 minutes ago\" # shows today's sunrise -30 minutes
# thisscript.sh sunrise \"2 hours ago\" # shows today's sunrise -2 hours

# Enter your location ID obtained from weather.yahoo.com URL 
LOCATION=733881

if [ ! $1 ]; then
echo "Parameter(s) missing!!!"
echo ""
echo "Example parameters:"
echo "thisscript.sh sunset # shows today's sunset"
echo "thisscript.sh sunrise # shows today's sunrise"
echo "thisscript.sh sunset \"30 minutes\" # shows today's sunset +30 minutes"
echo "thisscript.sh sunset \"2 hours\" # shows today's sunset +2 hours"
echo "thisscript.sh sunset \"30 minutes ago\" # shows today's sunset -30 minutes"
echo "thisscript.sh sunset \"2 hours ago\" # shows today's sunset -2 hours"
echo "thisscript.sh sunrise \"30 minutes\" # shows today's sunrise +30 minutes"
echo "thisscript.sh sunrise \"2 hours\" # shows today's sunrise +2 hours"
echo "thisscript.sh sunrise \"30 minutes ago\" # shows today's sunrise -30 minutes"
echo "thisscript.sh sunrise \"2 hours ago\" # shows today's sunrise -2 hours"
fi

case $1 in

sunset) 
SUNSET12H=`curl -s http://weather.yahooapis.com/forecastrss?w=${LOCATION}|grep astronomy|awk -F\" '{print $4}'`
SUNSET24H=`date --date="${SUNSET12H}" +%T`
date --date "${SUNSET24H} $2" +%R 
;;

sunrise)
SUNRISE12H=`curl -s http://weather.yahooapis.com/forecastrss?w=${LOCATION}|grep astronomy|awk -F\" '{print $2}'`
SUNRISE24H=`date --date="${SUNRISE12H}" +%T`
date --date "${SUNRISE24H} $2" +%R 
;;
esac
