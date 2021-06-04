#!/bin/bash

echo "installing GIT"
sudo apt install -y git

git clone https://github.com/Djarvur/go-swagger.git &&
    cd go-swagger/ && go install ./cmd/swagger/ && cd .. && rm -rf go-swagger/

