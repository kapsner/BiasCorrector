#!/bin/bash

# create folder to add to Dockerfile
mkdir addfolder
cd addfolder

# clone repository
git clone https://github.com/kapsner/BiasCorrector.git
cd BiasCorrector
git checkout latest
cd ..

# build image
cd ..
docker build -f Dockerfile -t biascorrector .

# remove addfolder
rm -rf ./addfolder

docker-compose -f docker-compose.local.yml up -d
