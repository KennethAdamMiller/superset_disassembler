#!/bin/bash


RQ3_folder=${HOME}/workspace/rq3_binaries/

fn_check() {
  f=${1}
  o=${RQ3_folder}/$(basename ${f})
  #if [ ! -f "${o}.lserr" ]; then
  cp ${f} ${o}_stripped
  strip ${o}_stripped
  objdump --no-show-raw-insn -d ${o}_stripped | grep ":" | cut -d ":" -f 1 | grep -e "[0-9a-f]"     | grep -v "[g-z]" > ${o}.lserr
  #fi
  if [ ! -f "${o}_stripped_addrs.txt" ]; then
      superset_disasm --target=${o}_stripped --save_addrs --tp_threshold=0.99
      mv $(basename ${f})_stripped_addrs.txt ${RQ3_folder}/
  fi
  if [ ! -f "${o}.truth" ]; then
      superset_disasm --target=${f} --ground_truth_bin=${f} --tp_threshold=0.99 --save_gt > ${o}.truth
  fi
  echo "$(basename ${f}) results: " 
  python ${HOME}/workspace/superset_disassembler/scripts/calc_fn.py ${o}.truth ${o}.lserr ${o}_stripped_addrs.txt
  echo ""
}

fn_check ${RQ3_folder}/gcc_coreutils_64_O0_chroot
fn_check ${RQ3_folder}/gcc_findutils_64_O0_xargs
rm ${RQ3_folder}/gcc_coreutils_64_O0_make-prime-list_stripped_addrs.txt
fn_check ${RQ3_folder}/gcc_coreutils_64_O0_make-prime-list
fn_check ${RQ3_folder}/gcc_coreutils_64_O0_true
fn_check ${RQ3_folder}/gcc_coreutils_64_O0_false
fn_check ${RQ3_folder}/gcc_coreutils_64_O0_printenv

