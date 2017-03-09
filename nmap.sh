#!/bin/bash

HOST=$1

#SO requiere root
echo -e "\e[1;4;31mSISTEMA OPERATIVO\e[0m"
sudo nmap -O $HOST
sleep 4


#RAPIDO
echo -e "\e[1;4;31mANALISIS RAPIDO\e[0m"
nmap -T4 -F $HOST
sleep 4

#TCP Connect Scan
echo -e "\e[1;4;31mPUERTOS TCP CONECTADOS\e[0m"
nmap -sT $HOST
sleep 4

#TCP Syn and UDP Scan requires root
echo -e "\e[1;4;31mTCP Y UDP\e[0m"
sudo nmap -sS -sU -PN $HOST
sleep 4

echo -e "\e[1;4;31mCIAO!!\e[0m"
