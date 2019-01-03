#!/bin/bash

mkdir ./RQ3

RQ3_folder=./RQ3

fn_check() {
  f=${1}
  o=${RQ3_folder}/$(basename ${f})
  objdump -no-show-raw-insn -no-symbolic-operands -no-leading-headers -d ${f} | grep ":" | cut -d ":" -f 1 | grep -e "[0-9a-f]"     | grep -v "[g-z]" > ${o}.lserr
  superset_disasm --target=${f} --ground_truth_bin=${f} --save_addrs --save_gt > ${o}.truth
}

#${HOME}//workspace/x86_64-binaries/elf//coreutils/gcc_coreutils_64_O0_make-prime-list

