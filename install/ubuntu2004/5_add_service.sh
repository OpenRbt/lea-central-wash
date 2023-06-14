#!/bin/bash

function GetDIR {
  echo "$(pwd)" | sed 's/\//\\\//g'
}

cd ../..
CUR_DIR=$(GetDIR)
echo "CUR_DIR is $CUR_DIR"
cd install/ubuntu2004

sed -i "s/{DIR}/$CUR_DIR/g" lea-central-wash.service
sed -i "s/{USER}/$USER/g" lea-central-wash.service
sudo cp lea-central-wash.service /etc/systemd/system/
sudo systemctl enable lea-central-wash

sed -i "s/{DIR}/$CUR_DIR/g" hal.service
sed -i "s/{USER}/$USER/g" hal.service
sudo cp hal.service /etc/systemd/system/
sudo systemctl enable hal
