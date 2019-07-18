#!/bin/bash

set -eu

gcloud auth configure-docker

make build-release STACK_FLAG=--docker
make prepare-image STACK_FLAG=--docker
make image-migrate STACK_FLAG=--docker
make image-api STACK_FLAG=--docker

docker tag nuitomo-api-migrate asia.gcr.io/nuitomo-c4587/nuitomo-api-migrate:latest
docker tag nuitomo-api asia.gcr.io/nuitomo-c4587/nuitomo-api:latest

docker push asia.gcr.io/nuitomo-c4587/nuitomo-api-migrate:latest
docker push asia.gcr.io/nuitomo-c4587/nuitomo-api:latest


# todo remoteでの作業を自動化
