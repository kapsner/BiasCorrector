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

printf "\n\nbuilding images ...\n"

mkdir -p addfolder/
cp -R ../../BiasCorrector/* addfolder/

printf "building image: $REGISTRY_PREFIX/$IMAGE_NAME$VERSION_TAG \n\n\n"
docker build -f ../Dockerfile -t "$REGISTRY_PREFIX/$IMAGE_NAME$VERSION_TAG" .

rm -rf addfolder/

printf "\n\npushing images ...\n"

printf "pushing image: $REGISTRY_PREFIX/$IMAGE_NAME \n\n\n"
docker push "$REGISTRY_PREFIX/$IMAGE_NAME$VERSION_TAG"

printf "\n\nfinished building an pushing all images ....\n"
