#!/bin/bash
eval $(opam env)
python3 scripts/analyze.py broker-service-$1
