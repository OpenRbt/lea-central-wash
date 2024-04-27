#!/bin/bash
echo "SAMPLE BUILD"
export TAG=v0.0.13
docker buildx build --build-arg BINARY=storage --platform=linux/amd64 -t romanfedyashov/lcw-storage:$TAG .
docker push romanfedyashov/lcw-storage:$TAG
