#!/bin/bash

cd ../..
git clone https://github.com/DiaElectronics/online_kasse
cd online_kasse
./build.sh
./deploy_kasse.sh
