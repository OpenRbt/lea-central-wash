#!/bin/bash

sudo echo "@xset s off" >> /etc/xdg/lxsession/LXDE-pi/autostart
sudo echo "@xset -dpms" >> /etc/xdg/lxsession/LXDE-pi/autostart
sudo echo "@xset s noblank" >> /etc/xdg/lxsession/LXDE-pi/autostart
sudo echo "@/home/pi/run.sh" >> /etc/xdg/lxsession/LXDE-pi/autostart

sudo echo "cd /home/pi/lea-central-wash/lazarusGUI" >> /home/pi/run.sh
sudo echo "./WashServerGUI" >> /home/pi/run.sh

cd /home/pi
chmod 777 /home/pi/run.sh
