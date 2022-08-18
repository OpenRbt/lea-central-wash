#!/bin/bash

go mod vendor
go mod tidy

go build ./cmd/storage
go build ./cmd/hal
