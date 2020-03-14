#!/bin/bash

cd ..
CURRENT_DIR=echo $(pwd) | sed 's/\//\\\//g'
cd install

sed -i "s/{DIR}/$CURRENT_DIR/g" lea-central-wash.service

systemctl enable lea-central-wash
