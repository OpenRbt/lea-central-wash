#!/bin/bash

function GetDIR {
  echo "$(pwd)" | sed 's/\//\\\//g'
}

cd ..
CUR_DIR=$(GetDIR)
echo "CUR_DIR is $CUR_DIR"
cd install

sed -i "s/{DIR}/$CUR_DIR/g" lea-central-wash.service
cp lea-central-wash.service /etc/systemd/system/
systemctl enable lea-central-wash
