#!/bin/bash

go mod vendor
go mod tidy

go build  -o storage.exe ./cmd/storage
