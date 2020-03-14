#!/bin/bash

./install_postgres.sh
./install_golang.sh
./build_project.sh
./add_service.sh
sudo ./remove_sleep.sh

./install_onlinekasse.sh
