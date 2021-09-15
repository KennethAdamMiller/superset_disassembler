#!/bin/bash
j1=python scripts/run_malamute.py &
j2=python scripts/run_client.py & 
j3=python scripts/run_client.py & 
wait ${j1}
wait ${j2}
wait ${j3}
