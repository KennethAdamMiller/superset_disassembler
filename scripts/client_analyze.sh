#!/bin/bash
eval $(opam env)
mkdir -p ${HOME}/.cache/
sudo chown $(id -u):$(id -u) ${HOME}/.cache
python3 scripts/analyze.py broker-service-$1
