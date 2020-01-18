#!/bin/bash

# create folder to add to Dockerfile
mkdir addfolder
cd addfolder

# clone repository
git clone -b v0.1.6 https://github.com/kapsner/rBiasCorrection.git
git clone -b v0.0.6 https://github.com/kapsner/BiasCorrector.git

# build image
cd ..
docker build -f Dockerfile -t biascorrector .

# remove addfolder
rm -rf ./addfolder

docker-compose -f docker-compose.local.yml up -d
