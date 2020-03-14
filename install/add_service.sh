#!/bin/bash

cd ..
sed -i "s/{DIR}/$(pwd)/g" lea-central-wash.service
cd install

systemctl enable lea-central-wash
