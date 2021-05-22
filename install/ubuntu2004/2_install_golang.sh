#!/bin/bash

echo "Never use golang from repos =) It is too old"
# sudo apt install golang


wget -o ~/go.tar.gz https://golang.org/dl/go1.16.4.linux-amd64.tar.gz
sudo rm -rf /usr/local/go && sudo  tar -C /usr/local -xzf ~/go.tar.gz
echo "export PATH=$PATH:/usr/local/go/bin" >> ~/.bashrc
export PATH=$PATH:/usr/local/go/bin

