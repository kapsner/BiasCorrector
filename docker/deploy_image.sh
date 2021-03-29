#!/bin/bash
# by Julian Gruendner

REGISTRY_PREFIX=$1 # z.B. docker.miracum.org/repositoryname
IMAGE_NAME=$2
VERSION_TAG=$3

if [ -n $VERSION_TAG ]; then
    VERSION_TAG=":$VERSION_TAG"
fi

printf "\n\nPlease insert your login credentials to registry: $REGISTRY_PREFIX ...\n"
docker login "https://$REGISTRY_PREFIX"

printf "\n\ncloning repo ...\n"
# create folder to add to Dockerfile
mkdir addfolder
cd addfolder

# clone repository
git clone -b development https://github.com/kapsner/rBiasCorrection.git
#git clone -b v0.1.4 https://github.com/kapsner/BiasCorrector.git
git clone -b development https://github.com/kapsner/BiasCorrector.git

# build image
cd ..

printf "\n\nbuilding images ...\n"

printf "building image: $REGISTRY_PREFIX/$IMAGE_NAME$VERSION_TAG \n\n\n"
docker build -f Dockerfile -t "$REGISTRY_PREFIX/$IMAGE_NAME$VERSION_TAG" .

# remove addfolder
rm -rf ./addfolder

printf "\n\npushing images ...\n"

printf "pushing image: $REGISTRY_PREFIX/$IMAGE_NAME \n\n\n"
docker push "$REGISTRY_PREFIX/$IMAGE_NAME$VERSION_TAG"

printf "\n\nfinished building an pushing all images ....\n"
