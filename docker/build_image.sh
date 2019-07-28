#!/bin/bash

docker build -f Dockerfile -t biascorrector .

docker-compose -f docker-compose.local.yml up -d
