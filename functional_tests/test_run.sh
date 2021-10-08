#!/bin/bash
echo "====$0===="
echo "int main(void){return 0;}" > "empty.c"
rm -f ${HOME}/.cache/bap/data/*
gcc empty.c -o empty
cp ./empty ./empty_stripped
strip ./empty_stripped
superset_disasm --target="empty_stripped" --ground_truth_bin="./empty" --save_addrs --enable_feature="${1}" --rounds=2 --tp_threshold="0.99" > empty_disasm.txt;
cat empty_disasm.txt
