#!/bin/bash

sudo echo "@xset s off" >> /etc/xdg/lxsession/LXDE-pi/autostart
sudo echo "@xset -dpms" >> /etc/xdg/lxsession/LXDE-pi/autostart
sudo echo "@xset s noblank" >> /etc/xdg/lxsession/LXDE-pi/autostart
sudo echo "@/home/pi/run.sh" >> /etc/xdg/lxsession/LXDE-pi/autostart

# system.d wants just do not work on raspbian.. in the ideal world .service file
# must contain wants: postgresql.service
# in the real raspbian it still starts before postgres and I do not have time to investigate it now
# so let's put some sleep
sudo echo "sleep 30" >> /home/pi/run.sh
:>/home/pi/lock.once
sudo echo "flock /home/pi/lock.once"
sudo echo "sudo systemctl restart lea-central-wash.service"
sudo echo "cd /home/pi/lea-central-wash/lazarusGUI" >> /home/pi/run.sh
sudo echo "./WashServerGUI" >> /home/pi/run.sh

cd /home/pi
chmod 777 /home/pi/run.sh
