#!/bin/bash
echo "SAMPLE BUILD"
export TAG=v0.0.6
docker buildx build --build-arg BINARY=hal --platform=linux/amd64 -t "romanfedyashov/lcw-hal:$TAG" .
docker push romanfedyashov/lcw-hal:$TAG
