#!/bin/bash

sudo ./install_postgres.sh
sudo ./install_golang.sh
sudo ./build_project.sh
sudo ./add_service.sh

./install_onlinekasse.sh
