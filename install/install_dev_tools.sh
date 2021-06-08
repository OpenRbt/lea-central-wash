#!/bin/bash

git clone https://github.com/go-swagger/go-swagger.git &&
    cd go-swagger/ && go install ./cmd/swagger/ && cd .. && rm -rf go-swagger/

