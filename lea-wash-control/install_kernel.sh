#!/bin/bash

echo "INSTALLING ADD-APT-REPOSITORY"
sudo apt install software-properties-common

echo "ADDING REPOSITORY"
sudo add-apt-repository ppa:morphis/anbox-support

echo "UPDATING ..."
sudo apt update

echo "INSTALLING KERNEL MODULES ..."
sudo apt install linux-headers-generic anbox-modules-dkms

echo "STARTING ASHMEM MODULE ..."
sudo modprobe ashmem_linux
echo "STARTING BINDER ..."
sudo modprobe binder_linux

