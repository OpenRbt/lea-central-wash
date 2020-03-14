#!/bin/bash

cd ..
CURRENT_DIR=$(pwd)
sed -i "s/{DIR}/$CURRENT_DIR/g" ./install/lea-central-wash.service
cd install

systemctl enable lea-central-wash
