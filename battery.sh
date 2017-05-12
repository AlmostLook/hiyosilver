#!/bin/bash

acpi -b | grep -P -o '[0-9]+(?=%)'

porciento=`acpi -b | grep -P -o '[0-9]+(?=%)'`
if [ $porciento -le 10 ]
then
    notify-send "Bateria Baja" "El nivel de bateria es del ${porciento}%!"
fi
