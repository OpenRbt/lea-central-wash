#!/bin/bash

sudo apt install fpc lazarus -y
cd ../lazarusGUI
./build.sh
./install.sh
