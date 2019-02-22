#!/bin/bash

mkdir -p addfolder/
cp -R ../BiasCorrector/* addfolder/

docker build -f Dockerfile -t biascorrector .

rm -rf addfolder/
